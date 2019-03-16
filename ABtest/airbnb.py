# -*- coding: utf-8 -*-
"""
Created on Fri Oct 27 14:22:27 2017

@author: yyang
"""

import pandas as pd
import os
import re
import numpy as np
import seaborn as sns
import matplotlib as mpl
import matplotlib.pyplot as plt

from sklearn.preprocessing import Imputer
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestRegressor
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import train_test_split

import nltk
from nltk import word_tokenize
from nltk.tokenize import RegexpTokenizer
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer

from gensim.corpora.dictionary import Dictionary
from gensim.models.tfidfmodel import TfidfModel
from gensim.models.ldamodel import LdaModel

import itertools
from collections import Counter
from collections import defaultdict

import json
import pyLDAvis.gensim



"""
read in data
"""

sns.set(style="ticks")
seed=42
np.random.seed(seed)
pd.options.display.max_columns = 20

dir_path = os.path.realpath('')
path = os.path.join(dir_path, 'data\\listings.csv')
df = pd.read_csv(path, header=0, index_col=0)
print("Dataset has {} rows, {} columns.".format(*df.shape))

"""
data exploration
"""

class sizeme():
    """ Class to change html fontsize of object's representation"""
    def __init__(self,ob, size, height=100):
        self.ob = ob
        self.size = size
        self.height = height
    def _repr_html_(self):
        repl_tuple = (self.size, self.height, self.ob._repr_html_())
        return u'<span style="font-size:{0}%; line-height:{1}%">{2}</span>'.format(*repl_tuple)


cols_to_keep = [
    'description', 
    'property_type', 'room_type', 'accommodates',
    'bathrooms', 'bedrooms', 'beds', 'square_feet',
    'price', 'cleaning_fee', 'guests_included', 'extra_people', 'minimum_nights',
    'availability_365', 'reviews_per_month', 'latitude', 'longitude', 'bed_type'
]
df = df[cols_to_keep]
show = [ 
    'property_type', 'room_type', 'accommodates',
    'bathrooms', 'square_feet',
    'price',  
    'availability_365', 'reviews_per_month', 'latitude', 'longitude', 'bed_type'
]
table_output = df[show]
print("Dataset has {} rows, {} columns.".format(*df.shape))
sizeme(table_output.head(), 80, 100)

# Clean numeric fields
num_feats = ['cleaning_fee', 'extra_people', 'price']
df[num_feats] = df[num_feats].replace('[\$,]', '', regex=True)
df[num_feats] = df[num_feats].apply(pd.to_numeric, errors='coerce')
print("Dataset has {} rows, {} columns.".format(*df.shape))

#Distribution of availability_365
%matplotlib inline

fig, axs = plt.subplots(ncols=2, figsize=(16, 4))
fig.suptitle('Distribution of availability (before and after removing part-time listings)')

# Before cleaning
x_axis=df['availability_365'].dropna()
sns.distplot(pd.Series(x_axis, name='Availability (before cleaning)'), ax=axs[0])

# Remove where availability less than 60 days or greater than 300 days
df = df.query('60 <= availability_365 <= 300')
print("Before cleaning, dataset has {} rows, {} columns.".format(*df.shape))

#After cleaning
x_axis=df['availability_365'].dropna()
sns.distplot(pd.Series(x_axis, name='Availability (after cleaning)'), ax=axs[1])
df = df.drop('availability_365', axis = 1)
print("After cleaning, dataset has {} rows, {} columns.".format(*df.shape))


#Distribution of price
%matplotlib inline

fig, axs = plt.subplots(ncols=2, figsize=(16, 4))
fig.suptitle('Distribution of max guests (before and after removing large listings > 10)', weight='bold', fontsize=12)

# Before cleaning
x_axis=df['accommodates'].dropna()
sns.distplot(pd.Series(x_axis, name='Max guests (before cleaning)'), ax=axs[0])

# Remove where price > 1000
condition = df[df['accommodates'] > 10]
rows_to_drop = condition.index
print("You dropped {} rows.".format(condition.shape[0]))
df = df.drop(rows_to_drop, axis=0)
print("Dataset has {} rows, {} columns.".format(*df.shape))

#After cleaning
x_axis=df['accommodates'].dropna()
sns.distplot(pd.Series(x_axis, name='Max guests (after cleaning)'), ax=axs[1])

#Distribution of price
%matplotlib inline

fig, axs = plt.subplots(ncols=2, figsize=(16, 4))
fig.suptitle('Distribution of price (before and after removing high-priced outliers)')

# Before cleaning
x_axis=df['price'].dropna()
sns.distplot(pd.Series(x_axis, name='Price (before cleaning)'), ax=axs[0])

# Remove where price > 1000
condition = df[df['price'] > 1000]
rows_to_drop = condition.index
print("You dropped {} rows.".format(condition.shape[0]))
df = df.drop(rows_to_drop, axis=0)
print("Dataset has {} rows, {} columns.".format(*df.shape))
#After cleaning
x_axis=df['price'].dropna()
sns.distplot(pd.Series(x_axis, name='Price  (after cleaning)'), ax=axs[1])


df['property_type'].value_counts()
categories_to_rename = ['Loft','Dorm','Guesthouse','Boutique hotel','Boat','Bungalow','Hostel',
                        'Cabin','Serviced apartment','Camper/RV','Villa','Condominium','Chalet','Parking Space',
                        'Tent','Yurt','Igloo','Ryokan (Japan)','Castle', 'Townhouse', 'Lighthouse']
df['property_type'].loc[df['property_type'].isin(categories_to_rename)] = 'Other'

categories_to_rename = ['Pull-out Sofa', 'Futon', 'Couch', 'Airbed']
df['bed_type'].loc[df['bed_type'].isin(categories_to_rename)] = 'Non-Real Bed'

#Distribution of yield, availability_365, reviews_per_month
%matplotlib inline

fig, axs = plt.subplots(ncols=2, figsize=(16, 4))
fig.suptitle('Distribution of variables for calculation of yield')
x_axis=df['price'].dropna()
sns.distplot(pd.Series(x_axis, name=x_axis.name), ax=axs[0])
x_axis=df['reviews_per_month'].dropna()
sns.distplot(pd.Series(x_axis, name=x_axis.name), ax=axs[1])

# Calculate yield

# Average length of stay (by city) X Price ('price') X No. of reviews('reviews_per_month') / Review rate('50%')
avg_length_of_stay_london = 3
review_rate = 0.5
df['price'] = df['price'] + df['cleaning_fee']
df['yield'] = avg_length_of_stay_london * df['price'] * (df['reviews_per_month'] / review_rate) * 12

cols_to_drop = ['cleaning_fee']
df = df.drop(cols_to_drop, axis = 1)
print("Dataset has {} rows, {} columns.".format(*df.shape))


%matplotlib inline
fig, ax = plt.subplots(figsize=(15,6))
fig.suptitle('Distribution of Yield (showing only <£20,000/month)')
x_axes = df['yield'].dropna()
x = pd.Series(x_axis, name=x_axis.name)
ax = sns.distplot(x)


from sklearn.base import TransformerMixin

class DataFrameImputer(TransformerMixin):

    def __init__(self):
        """Impute missing values.

        Columns of dtype object are imputed with the most frequent value 
        in column.

        Columns of other types are imputed with mean of column.

        """
    def fit(self, X, y=None):

        self.fill = pd.Series([X[c].value_counts().index[0]
            if X[c].dtype == np.dtype('O') else X[c].mean() for c in X],
            index=X.columns)

        return self

    def transform(self, X, y=None):
        return X.fillna(self.fill)       
    
 # Remove missing yield values
df = df.dropna(subset=['yield'])

# Remove square_feet as it is too sparse.
df = df.drop('square_feet', axis=1)

# Impute most frequent for categorical features
categorical_feats = df.select_dtypes(include=['object']).columns
df[categorical_feats] = DataFrameImputer().fit_transform(df[categorical_feats])

# Impute median for numerical features
num_feats = df.select_dtypes(include=[np.number]).columns
imp = Imputer(missing_values=np.nan, strategy='median', axis=0)
df[num_feats] = imp.fit_transform(df[num_feats])

print("Dataset has {} rows, {} columns.".format(*df.shape))
   

"""
Topic modelling for listing description text
"""
# Seperate NLP features
nlp_feats = ['description']
corpus = df[nlp_feats]
df = df.drop(nlp_feats, axis = 1)
print("Dataset has {} rows, {} columns.".format(*df.shape))

def preprocess_text(corpus):
    """Takes a corpus in list format and applies basic preprocessing steps of word tokenization,
     removing of english stop words, lower case and lemmatization."""
    processed_corpus = []
    english_words = set(nltk.corpus.words.words())
    english_stopwords = set(stopwords.words('english'))
    wordnet_lemmatizer = WordNetLemmatizer()
    tokenizer = RegexpTokenizer(r'[\w|!]+')
    for row in corpus:
        word_tokens = tokenizer.tokenize(row)
        word_tokens_lower = [t.lower() for t in word_tokens]
        word_tokens_lower_english = [t for t in word_tokens_lower if t in english_words or not t.isalpha()]
        word_tokens_no_stops = [t for t in word_tokens_lower_english if not t in english_stopwords]
        word_tokens_no_stops_lemmatized = [wordnet_lemmatizer.lemmatize(t) for t in word_tokens_no_stops]
        processed_corpus.append(word_tokens_no_stops_lemmatized)
    return processed_corpus

def nlp_model_pipeline(processed_corpus):
    """Takes processed corpus and produce dictionary, doc_term_matrix and LDA model"""
    # Creates the term dictionary (every unique term in corpus is assigned an index)
    dictionary = Dictionary(processed_corpus)
    # Convert corpus into Document Term Matrix using dictionary prepared above
    doc_term_matrix = [dictionary.doc2bow(listing) for listing in processed_corpus]    
    return dictionary, doc_term_matrix


def LDA_topic_modelling(doc_term_matrix, dictionary, num_topics=3, passes=2):
    # Create an object for LDA model and train it on Document-Term-Matrix
    LDA = LdaModel
    ldamodel = LDA(doc_term_matrix, num_topics=num_topics, id2word = dictionary, passes=passes)
    return ldamodel

def add_topics_to_df(ldamodel, doc_term_matrix, df, new_col, num_topics):
    # Convert into Per-document topic probability matrix:
    docTopicProbMat = ldamodel[doc_term_matrix]
    docTopicProbDf = pd.DataFrame(index=df.index, columns=range(0, num_topics))
    for i, doc in enumerate(docTopicProbMat):
        for topic in doc:
            docTopicProbDf.iloc[i, topic[0]] = topic[1]
    docTopicProbDf[new_col] = docTopicProbDf.idxmax(axis=1)
    df_topics = docTopicProbDf[new_col]
    # Merge with df
    df_new = pd.concat([df, df_topics], axis=1)
    return df_new


corpus.head()

%%time
corpus_description = corpus['description'].astype(str)
processed_corpus_description = preprocess_text(corpus_description)
dictionary_description, doc_term_matrix_description = nlp_model_pipeline(processed_corpus_description)

%%time
ldamodel_description = LDA_topic_modelling(doc_term_matrix_description, dictionary_description, num_topics=3, passes=1)

"""
Exploratory Visualization
"""
p = pyLDAvis.gensim.prepare(ldamodel_description, doc_term_matrix_description, dictionary_description)
pyLDAvis.save_html(p, 'lda.html')


"""
methodology
"""
df.describe()
cols_to_drop = ['price', 'reviews_per_month']
df_final_viz = df[cols_to_drop]
df = df.drop(cols_to_drop, axis = 1)
print("Dataset has {} rows, {} columns.".format(*df.shape))


# Dummy encoding
categorical_feats = ['property_type', 'room_type', 'bed_type']
df = pd.get_dummies(df, columns=categorical_feats, drop_first=False)
print("Dataset has {} rows, {} columns.".format(*df.shape))

# Create response and target variable
target = 'yield'
X = df.drop(target, axis=1)
y = df[target]

test_size = 0.3
seed = 42

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=test_size, random_state=seed)

"""
linear regression
"""
from sklearn import linear_model
from sklearn.metrics import mean_squared_error, r2_score

# Create linear regression object
regr = linear_model.LinearRegression()
# Train the model using the training sets
regr.fit(X_train, y_train)

# Make predictions using the testing set
regr_y_pred = regr.predict(X_test)

# The mean squared error
print("Mean squared error: %.2f" % mean_squared_error(y_test, regr_y_pred))
# Explained variance score: 1 is perfect prediction
print('Variance score: %.2f' % r2_score(y_test, regr_y_pred))

from sklearn.tree import DecisionTreeRegressor

# Fit regression model
dt_regr = DecisionTreeRegressor(random_state=seed, max_depth=5)
dt_regr.fit(X_train, y_train)

# Make prediction
dt_y_pred = dt_regr.predict(X_test)
# The mean squared error
print("Mean squared error: %.2f" % mean_squared_error(y_test, dt_y_pred))
# Explained variance score: 1 is perfect prediction
print('Variance score: %.2f' % r2_score(y_test, dt_y_pred))

%%time
rf_regr = RandomForestRegressor(random_state=seed, bootstrap=True, criterion='mse', max_depth=10, 
                           max_features='auto', min_samples_split=4, n_estimators=150)

rf_regr.fit(X_train, y_train)

# Make prediction
rf_y_pred = rf_regr.predict(X_test)
# The mean squared error
print("Mean squared error: %.2f" % mean_squared_error(y_test, rf_y_pred))
# Explained variance score: 1 is perfect prediction
print('Variance score: %.2f' % r2_score(y_test, rf_y_pred))


"""
Refinement
"""
df = add_topics_to_df(ldamodel_description, doc_term_matrix_description, df, new_col='topics_description', num_topics=3)

# Rename based on understanding of topics
df['topics_description'].replace({0:'Luxury', 1:'Business', 2:'Budget'},inplace=True)
df = pd.get_dummies(df, columns=['topics_description'], drop_first=False)

print("Dataset has {} rows, {} columns.".format(*df.shape))

# Create response and target variable
X = df.drop(target, axis=1)
y = df[target]
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=test_size, random_state=seed)


%%time
rf_regr.fit(X_train, y_train)

# Make prediction
rf_y_pred = rf_regr.predict(X_test)
# The mean squared error
print("Mean squared error: %.2f" % mean_squared_error(y_test, rf_y_pred))
# Explained variance score: 1 is perfect prediction
print('Variance score: %.2f' % r2_score(y_test, rf_y_pred))

#Tuning the model
param_grid = { "n_estimators"      : [175, 200],
           "criterion"         : ['mse'],
           "max_features"      : ['auto'], #auto, sqrt, log2, int/n_feature
           "max_depth"         : [10, 14],
           "min_samples_split" : [6, 10] ,
           "bootstrap": [True]}

%%time
rf = RandomForestRegressor(random_state=seed)

rf_cv = GridSearchCV(rf, param_grid, cv=5)
rf_cv.fit(X_train, y_train)
# Make prediction
rf_y_pred = rf_cv.predict(X_test)
# The mean squared error
print("Mean squared error: %.2f" % mean_squared_error(y_test, rf_y_pred))
# Explained variance score: 1 is perfect prediction
print('Variance score: %.2f' % r2_score(y_test, rf_y_pred))
# Best params
print("Tuned Model Parameters: {}".format(rf_cv.best_params_))

"""
Results
"""
"""
model evaluate and validation
"""
# Change variables
random_state = 59

X = df.drop(target, axis=1)
y = df[target]
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=test_size, random_state=seed)

# Fit and make prediction
rf_regr.fit(X_train, y_train)
rf_y_pred = rf_regr.predict(X_test)

# The mean squared error
print("Mean squared error: %.2f" % mean_squared_error(y_test, rf_y_pred))
# Explained variance score: 1 is perfect prediction
print('Variance score: %.2f' % r2_score(y_test, rf_y_pred))


# Change variables
test_size = 0.2

X = df.drop(target, axis=1)
y = df[target]
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=test_size, random_state=seed)

# Fit and make prediction
rf_regr.fit(X_train, y_train)
rf_y_pred = rf_regr.predict(X_test)

# The mean squared error
print("Mean squared error: %.2f" % mean_squared_error(y_test, rf_y_pred))
# Explained variance score: 1 is perfect prediction
print('Variance score: %.2f' % r2_score(y_test, rf_y_pred))


# Change inputs - accommodates
test_data = X_test.loc[1400514]
print(rf_cv.predict(test_data.reshape(1, -1)))

test_data['accommodates'] = 2
print(rf_cv.predict(test_data.reshape(1, -1)))

test_data['accommodates'] = 6
print(rf_cv.predict(test_data.reshape(1, -1)))

#Display feature importance
def feature_importance(model, trainData, display_n_rows):
    """Display feature importance & weighting for tree based model"""
    fi = model.feature_importances_*100
    feat_imp = pd.DataFrame(list(zip(fi,trainData.columns.values)))
    feat_imp = feat_imp.sort_values(by=0, axis=0, ascending=False)
    feat_imp.columns = ['importance %', 'feature']
    print(feat_imp[:display_n_rows])
    
#Display features & weighting
rf_best = rf_cv.best_estimator_
feature_importance(rf_best, X_train, 20)
    
df2 = df.join(df_final_viz)
nlp_feats = ['topics_description_Budget', 'topics_description_Business', 'topics_description_Luxury']
df2['topics_description'] = df2[nlp_feats].idxmax(axis=1)
df2['topics_description'] = df2['topics_description'].str.replace('topics_description_', '')
    
df3 = df2[['topics_description', 'price', 'reviews_per_month']].groupby(['topics_description']).mean()
df3['count'] = df2[['yield', 'topics_description']].groupby(['topics_description']).count()['yield']
df3['reviews_per_year'] = df3['reviews_per_month']*12
df3['color'] = ['R', 'G', 'B']

sns.palplot(sns.color_palette("Set2", 8))

plt.figure(figsize=(9, 6))

# Unique category labels:
color_labels = df3['color'].unique()

# List of RGB triplets
rgb_values = sns.color_palette("Set2", 8)

# Map label to RGB
color_map = dict(zip(color_labels, rgb_values))

# Finally use the mapped values
plt.scatter(df3['reviews_per_year'], df3['price'], c=df3['color'].map(color_map), s=df3['count']*1.5)

plt.ylim([100,200])
plt.xlim([16,30])

plt.xlabel('Avg. no. of reviews / year')
plt.ylabel('Avg. price / night (£)')
plt.title('Average price and frequency of reviews by description topic', weight='bold', fontsize=14)

for i, txt in enumerate(df3.index):
    plt.annotate(txt, (df3['reviews_per_year'].iat[i]-0.3,df3['price'].iat[i]), fontsize=11, weight='bold')
    
    




