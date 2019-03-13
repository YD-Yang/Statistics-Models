# -*- coding: utf-8 -*-
"""
Created on Tue Jan  9 10:13:49 2018

@author: yyang
"""



import os 
import sys 
import pandas as pd
import numpy as np 
import pystan 
import matplotlib.pyplot as plt
import pickle
from datetime import datetime 
from scipy.stats import gaussian_kde
from hashlib import md5


###############################################################################
###############################################################################
#data exploration
transactions = pd.read_csv("transactions.csv")
transactions['date'] = pd.to_datetime(transactions['date'])
transactions.head(20)
transactions.shape
print( transactions.groupby(['OID_CONSUMER_DIM']).size().shape)
print( transactions['date'].min(), transactions['date'].max())

# Time series of the number of transactions (daily) 
ts_transactions = transactions.groupby(['date']).size()
plt.ylabel('Transaction Count') 
ts_transactions.plot()


# Let's take a look at the typical inter-purchase time and IPT distribution 

def shift_date(x): 
    x['shifted_date'] = x['date'].shift(-1) 
    return x

# We'll apply a shift of -1 between the 'date' column and a newly shifted date column shift_date. 
# That way, we'll be able to subtract date from shifted_date at the customer level and compute 
# the inter purchase time (IPT) directly : 
transactions_tmp = transactions.sort_values(['date']).\
                        groupby(['OID_CONSUMER_DIM'], as_index=True).apply(shift_date)    

# Let's re-order by customer and date : 
transactions_tmp.sort_values(['OID_CONSUMER_DIM','date'], ascending=True, inplace=True)
transactions_tmp.dropna(inplace=True)

# Compute the IPT in days : 
transactions_tmp['IPT'] = (transactions_tmp['shifted_date'] - transactions_tmp['date']).apply(lambda x : x.days)

transactions_tmp.head(15)

# let's print the mean IPT. Our training period of 365 days is long enough. 

print(transactions_tmp['IPT'].mean())

# Distribution of IPT : 
transactions_tmp['IPT'].hist()
plt.xlabel('IPT (days)') 
plt.ylabel('Number of Purchases') 
plt.xlim([0,200])


# Let's look at the distribution of the number of purchases per customer : 
n_purchases = transactions.groupby(['OID_CONSUMER_DIM']).size()
print(n_purchases.min(axis=0), n_purchases.max(axis=0))
n_purchases.hist(bins=(n_purchases.max(axis=0) - n_purchases.min(axis=0)) + 1)
plt.xlabel('Number of Purchases') 
plt.ylabel('Number of Customers')


###############################################################################
###############################################################################
#Compute  recency-frequency-monetary value (RFM) dataframe 
#Recency : time between first and last transaction
#Frequency : here frequency really refers to repeat frequency, i.e. the number of purchases beyond the initial one. i.e. repeat frequency = purchase counts - 1 )
#monetary value : mean of all the transactions in the training periods
#T : time between first purchase and end of calibration period
# Select calibration and holdout periods 

# Lets select a training period of one year and and a holdout period of 6 months. 
end_calibration = pd.to_datetime('2017-07-30')
train = transactions[transactions.date <= end_calibration]
holdout = transactions[transactions.date > end_calibration]

#bin the transaction
# Sum the monetary value by customer and by date : 
train2 = train.sort_values(['date'], ascending=True).groupby(['OID_CONSUMER_DIM', 'date'], 
                                                             as_index=False)['TOTAL_PURCHASE_AMT'].sum()
train2.head()

# let's define a single function that can be applied at the customer level and 
# compute all the relevant RFM quantities at once : 
def compute_rfm(x, end_calibration): 
    x['recency'] = (x['date'].max() - x['date'].min()).days
    x['frequency'] = x['date'].count()-1
    x['T'] = (end_calibration - x['date'].min()).days
    x['monetary_value'] = x['TOTAL_PURCHASE_AMT'].mean()
    return x

# use the function compute_rfm to compute recency, frequency, T and monetary value 
# for each group (each customer). 
train3 = train2.groupby(['OID_CONSUMER_DIM']).apply(lambda x: compute_rfm(x, end_calibration))

# lets take the first row for each customer and only the relevant columns of interest. 
rfm = train3[['OID_CONSUMER_DIM', 'recency', 'frequency', 'T', 'monetary_value']].groupby(['OID_CONSUMER_DIM']).first()
rfm.describe()

###############################################################################
###############################################################################
#Train a simple Pareto/NBD Model over the training/holdout period
paretonbd_model="""
data{
int<lower=0> n_cust; //number of customers 
vector<lower=0>[n_cust] x; 
vector<lower=0>[n_cust] tx; 
vector<lower=0>[n_cust] T; 
}

parameters{
// vectors of lambda and mu for each customer. 
// Here I apply limits between 0 and 1 for each 
// parameter. A value of lambda or mu > 1.0 is unphysical 
// since you don't enough time resolution to go less than 
// 1 time unit. 
vector <lower=0,upper=1.0>[n_cust] lambda; 
vector <lower=0,upper=1.0>[n_cust] mu;

// parameters of the prior distributions : r, alpha, s, beta. 
// for both lambda and mu
real <lower=0>r;
real <lower=0>alpha;
real <lower=0>s;
real <lower=0>beta;
}

model{

// temporary variables : 
//vector[n_cust] like1;
//vector[n_cust] like2;
    
    
// Establishing hyperpriors on parameters r, alpha, s, and beta. 
r ~ normal(0.5,0.1);
alpha ~ normal(10,1);
s ~ normal(0.5,0.1);
beta ~ normal(10,1);

// Establishing the Prior Distributions for lambda and mu : 
lambda ~ gamma(r,alpha); 
mu ~ gamma(s,beta);

// The likelihood of the Pareto/NBD model : 
//like1 = x .* log(lambda) + log(mu) - log(mu+lambda) - tx .* (mu+lambda);
//like2 = (x + 1) .* log(lambda) - log(mu+lambda) - T .* (lambda+mu);

// Here we increment the log probability density (target) accordingly 
target+= log(exp( x .* log(lambda) + log(mu) - log(mu+lambda) - tx .* (mu+lambda))+exp((x + 1) .* log(lambda) - log(mu+lambda) - T .* (lambda+mu)));
}
"""

# here's the data we will provide to STAN : 
data={'n_cust':len(rfm),
    'x':rfm['frequency'].values,
    'tx':rfm['recency'].values,
    'T':rfm['T'].values
}


# STAN models can take a while to fit. Let's pickle the model to disk as a precautionary measure. 
# We can always read the model from disk later on. Note that this file is sizable > 100 MB. 

# Utility function to pull a stan model that has been pickled. 
# from pystan docs : https://pystan.readthedocs.io/en/latest/avoiding_recompilation.html
def stan_cache(model_code, model_name=None, **kwargs):
    """Use just as you would `stan`"""
    code_hash = md5(model_code.encode('ascii')).hexdigest()
    if model_name is None:
        cache_fn = 'cached-model-{}.pkl'.format(code_hash)
    else:
        cache_fn = 'cached-{}-{}.pkl'.format(model_name, code_hash)
    try:
        sm = pickle.load(open(cache_fn, 'rb'))
    except:
        sm = pystan.StanModel(model_code=model_code)
        with open(cache_fn, 'wb') as f:
            pickle.dump(sm, f)
    else:
        print("Using cached StanModel")
    return sm.sampling(**kwargs)

iterations = 100
warmup = 50

# I recommend training for several 1000's iterations. Here we run the STAN model : 
pareto_nbd_fit = stan_cache(paretonbd_model, model_name='paretonbd_model',  \
                            data=data, chains=1, iter=iterations, warmup=warmup)

# Here we'll extract the traces for the lambda and mu parameters. We get the posterior 
# distribution of these parameters for "free" when using STAN. 
trace = pareto_nbd_fit.extract()
lambdas = trace['lambda']
mus = trace['mu']


###############################################################################
###############################################################################
#Comparisons Between Model Predictions and Training Set Observations
# Here I take the expectation (mean) value of E[X(t) | lambda, mu] over the trace values of lambda and mu : dt_train = 302.0 # 12 months 
training_predictions = (lambdas/mus-lambdas/mus*np.exp(-mus*dt_train)).mean(axis=0)
rfm['model_train_count'] = training_predictions

rmse_train_count = (rfm['model_train_count'] - rfm['frequency']).apply(lambda x : x*x)
rmse_train_count = np.sqrt(rmse_train_count.sum()/len(rfm))
print('RMSE =', rmse_train_count)

def plot_scatter(dataframe, colx, coly, xlabel='Observed Counts', 
                 ylabel='Predicted Counts', 
                 xlim=[0,15], ylim=[0,15], density=True): 
    """This function will plot a scatter plot of colx on the x-axis vs coly on the y-axis. 
    If you want to add a color to indicate the density of points, set density=True
    
    Args : 
        - dataframe (dataframe) : pandas dataframe containing the data of interest 
        - colx (str) : name of the column you want to put on the x axis 
        - coly (str) : same but for the y axis 
        - xlabel (str) : label to put on the x axis 
        - ylabel (str) : same for y axis 
        - xlim (list) : defines the range of x values displayed on the chart 
        - ylim (list) same for the y axis. 
        - density (bool) : set True to add color to indicate density of point. 
        
    """

    if not density : 
        plt.scatter(dataframe[colx].values, dataframe[coly].values)
    else:
        xvals = dataframe[colx].values
        yvals = dataframe[coly].values
        xy = np.vstack([xvals, yvals])
        z = gaussian_kde(xy)(xy)
        plt.scatter(xvals, yvals, c=z, s=10, edgecolor='')
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.plot(np.linspace(xlim[0], xlim[1], 100), 
             np.linspace(ylim[0], ylim[1], 100), 
             color='black')
    plt.xlim(xlim)
    plt.ylim(ylim)
    plt.plot()

plot_scatter(rfm, 'frequency', 'model_train_count')

###############################################################################
###############################################################################
#Comparisons Between Predictions and the Holdout (validation) Set Observations
def prob_alive_at_T(lam, mu, t_x, T): 
    """Computes the probability of being alive at T given lambda, mu, t_x, and T"""
    return 1 / ( 1 + mu / (mu + lam) * (np.exp((lam + mu) * (T - t_x)) - 1) )

# Predictions made over the holdout period of 6 months : 
dt_hold = 65 # 6 months 

# Here we extract recency and T values: 
tmp = rfm['T'].values 
T_values = np.tile(tmp, [iterations - warmup, 1])
tmp2 = rfm['recency'].values 
recency_values = np.tile(tmp2, [iterations - warmup, 1])

# Holdout counts predictions : 
holdout_predictions = ((lambdas/mus - lambdas/mus*np.exp(-mus*dt_hold)) * \
                                prob_alive_at_T(lambdas, mus, recency_values, T_values)).mean(axis=0)

#holdout_predictions = (lambdas/mus - lambdas/mus*np.exp(-mus*dt_hold)) * prob_alive_at_t(lambdas, mus, t_x, T)
rfm['model_holdout_count'] = np.asarray(holdout_predictions)


# lets look at the observed number of transactions during the same time period : 
# counts per customer per date : 
holdout_counts = holdout.groupby(['OID_CONSUMER_DIM', 'date'], as_index=False).size().reset_index()

# counts per customer : 
holdout_counts = holdout_counts.groupby(['OID_CONSUMER_DIM']).size()

# Let's merge with the rfm object. 
rfm_with_holdout = rfm.merge(pd.DataFrame(holdout_counts), how='left', left_index=True, right_index=True)
rfm_with_holdout.rename(columns={0:'obs_holdout_count'}, inplace=True)
rfm_with_holdout.fillna(0, inplace=True)


# Let's now plot the data : 
rmse_holdout_count=(rfm_with_holdout['model_holdout_count'] - rfm_with_holdout['obs_holdout_count']).apply(lambda x :x*x)
rmse_holdout_count=np.sqrt(rmse_holdout_count.sum()/len(rfm_with_holdout))
print('RMSE =', rmse_holdout_count)
plot_scatter(rfm_with_holdout, 'obs_holdout_count', 'model_holdout_count')

###############################################################################
###############################################################################
#Training a simple Gamma-Gamma model on the monetary value
# This gamma-gamm model follows the Fader et al. (2004) Gamma-Gamma model closely. 
# Again, this model can take a while to train. Recommend a few 1000's iterations. 

gamma_gamma_model="""
data {
    // this is the data we pass to STAN : 
    int<lower=1> n_cust;         // number of customers 
    vector[n_cust] x;            // frequency + 1 
    vector[n_cust] mx;           // average purchase amount for each customer 
}

parameters {
    // These are the model parameters : 
    real <lower=0>p;             // scale parameter of the gamma distribution. Note that 
                                 // this parameter is not a vector. All customers will have the 
                                 // same value of p. 
    vector<lower=0> [n_cust] v;   // shape parameter of the gamma distribution (nu)
    real <lower=0>q;             // shape parameter of the gamma prior distribtion on v 
    real <lower=0>y;             // scale parameter of the gamma prior distribution on v 
}

transformed parameters {
    vector<lower=0> [n_cust] px;
    vector<lower=0> [n_cust] nx; 
    px <- p * x;                 // getting px from p and x 
    for (i in 1:n_cust) 
        nx[i] <- v[i] * x[i]; 
}

model {
    p ~ exponential(0.1);    // prior distribution on p
    q ~ exponential(0.5);    // hyperprior distribution on q 
    y ~ exponential(0.1);    // hyperprior distribution on y 
//    v ~ gamma(q, q ./ y);    // prior distribution on nu  
//    mx ~ gamma(px, v);       // likelihood function 
    v ~ gamma(q,y); 
    mx ~ gamma(px,nx); 
}
"""

# here's the data we will provide to STAN : 
data_gg={'n_cust':len(rfm),
    'x':rfm['frequency'].values+1.0,
    'mx':rfm['monetary_value'].values
     }

# I recommend training for several 1000's iterations. 
gamma_gamma_fit = stan_cache(gamma_gamma_model, model_name='gamma_gamma_model', \
                                  data=data_gg, chains=1, iter=100, warmup=50)

# Here I extract the model parameters from the fit 
trace_gg = gamma_gamma_fit.extract()
nu = trace_gg['v']
p = trace_gg['p']
gamma = trace_gg['y']
q = trace_gg['q']

# Now let's compute E(M) and join it to the rfm object :
pvalues = np.tile(np.array(p).T,(len(rfm),1))
E_M = (pvalues / nu.T).mean(axis=1)

# Now let's compute E(M) and join it to the rfm object : 
rfm['E_M'] = E_M
rfm[['monetary_value', 'E_M']].head()

# Let's explore the results : 
plot_scatter(rfm,'monetary_value','E_M', 
             xlabel='Average Order Value in Training Period ($)', 
             ylabel='E(M) ($)', 
             xlim=[0,200], ylim=[0,200])


#Comparisons between E(M) and observed mean in holdout/validation period
# Let's compute the observed mean transaction value per customer in the holdout period : 
holdout_value = holdout.groupby(['OID_CONSUMER_DIM', 'date'], as_index=False)['TOTAL_PURCHASE_AMT'].sum().reset_index()
holdout_value = holdout_value[['OID_CONSUMER_DIM', 'TOTAL_PURCHASE_AMT']].groupby(['OID_CONSUMER_DIM'])['TOTAL_PURCHASE_AMT'].mean()
holdout_value=pd.DataFrame(holdout_value)
holdout_value.rename(columns={'TOTAL_PURCHASE_AMT':'obs_holdout_monetary_value'}, inplace=True)
holdout_value.head()


# merge with rfm object : 
rfm_w_holdout_value  = rfm.merge(holdout_value, how='left', left_index=True, right_index=True)
rfm_w_holdout_value.fillna(0,inplace=True)

plot_scatter(rfm_w_holdout_value,'obs_holdout_monetary_value','E_M', 
             xlabel='Average Order Value in Validation Period ($)', 
             ylabel='E(M) ($)', 
             xlim=[0,200], ylim=[0,200])

rfm['E_M'].median()
rfm['E_M'].mean()
holdout_value.mean()


#Finally, computing the CLV in the holdout period and comparing with with model predictions
# compute both modeled and observed CLV in the holdout period : 

rfm['model_holdout_clv'] = rfm_with_holdout['model_holdout_count'] * rfm['E_M']
rfm['obs_holdout_clv'] = rfm_with_holdout['obs_holdout_count'] * rfm_w_holdout_value['obs_holdout_monetary_value']
rmse_holdout_clv = (rfm['model_holdout_clv'] - rfm['obs_holdout_clv'])* \
                   (rfm['model_holdout_clv'] - rfm['obs_holdout_clv'])
rmse_holdout_clv = np.sqrt(rmse_holdout_clv.sum()/len(rfm))

# plot the final results : 
print('RMSE =', rmse_holdout_clv)
plot_scatter(rfm, 'obs_holdout_clv', 'model_holdout_clv',
             xlabel='Observed Value in the Holdout Period',
             ylabel='Modeled Value in the Holdout Period', 
             xlim=[0,300.0],ylim=[0,300.0])




