# -*- coding: utf-8 -*-
"""
Created on Mon Dec  7 17:41:49 2020

@author: rj299
"""

import os
import numpy as np
import pandas as pd

from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LogisticRegressionCV
from sklearn.metrics import roc_curve, auc
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report

# for finding the most common elements
from collections import Counter
from itertools import chain

import matplotlib.pyplot as plt
from IPython import get_ipython
get_ipython().run_line_magic('matplotlib', 'inline')

#%%
# load data
os.chdir('e:/Ruonan/Projects in the lab/mdm_covid19/behavior/mdm_covid19_data')

data = pd.read_csv('data_for_classification.csv')

print(data.head)

#%%
# columns of high/low uncertainty attitude
# take the upper 40% and lower 40%
# print(data.columns)

quant= 0.1
lower_q = data.quantile(quant, axis = 0)
upper_q = data.quantile(1-quant, axis = 0)


data['risk.mon.label'] = np.nan
for i in range(data.shape[0]):
    if data['risk.mon'].iloc[i] <= lower_q['risk.mon']:        
        data['risk.mon.label'].iloc[i] = 0
    elif data['risk.mon'].iloc[i] >= upper_q['risk.mon']:
        data['risk.mon.label'].iloc[i] = 1
            
    
data['risk.mon.label']

data['risk.med.label'] = np.nan
for i in range(data.shape[0]):
    if data['risk.med'].iloc[i] <= lower_q['risk.med']:        
        data['risk.med.label'].iloc[i] = 0
    elif data['risk.med'].iloc[i] >= upper_q['risk.med']:
        data['risk.med.label'].iloc[i] = 1

data['risk.med.label']

data['ambig_corr.mon.label'] = np.nan
for i in range(data.shape[0]):
    if data['ambig_corr.mon'].iloc[i] <= lower_q['ambig_corr.mon']:        
        data['ambig_corr.mon.label'].iloc[i] = 0
    elif data['ambig_corr.mon'].iloc[i] > upper_q['ambig_corr.mon']:
        data['ambig_corr.mon.label'].iloc[i] = 1
            
    
data['ambig_corr.mon.label']

data['ambig_corr.med.label'] = np.nan
for i in range(data.shape[0]):
    if data['ambig_corr.med'].iloc[i] <= lower_q['ambig_corr.med']:        
        data['ambig_corr.med.label'].iloc[i] = 0
    elif data['ambig_corr.med'].iloc[i] >= upper_q['ambig_corr.med']:
        data['ambig_corr.med.label'].iloc[i] = 1
            
    
data['ambig_corr.med.label']

#%% label error
plt.figure()
plt.hist(data['error.med'])
plt.show()

plt.figure()
plt.hist(data['error.mon'])
plt.show()

data['error.mon.label'] = np.nan
data['error.mon.label'][data['error.mon'] > 0] = 1
data['error.mon.label'][data['error.mon'] == 0] = 0
sum(data['error.mon.label']==0)
sum(data['error.mon.label'] == 1)


data['error.med.label'] = np.nan
data['error.med.label'][data['error.med'] > 0] = 1
data['error.med.label'][data['error.med'] == 0] = 0
sum(data['error.med.label']==0)
sum(data['error.med.label'] == 1)

#%% logistic regression with cross-validation


# classifying monetary ambiguity attitude

# exclude subjects with too many errors
data_classify = data[(data['error.mon'] < 0.5) & (data['ambig_corr.mon.label'].notnull())]
data_classify.columns
data_classify

# classify errors, do not exclude
data_classify = data[data['error.mon.label'].notnull()]

y = data_classify['error.med.label']
print(y)

y = data_classify['ambig_corr.mon.label']
print(y)

X = data_classify.iloc[:,1:69]
print(X)

# number of subjects
print(X.shape)

clf_cv = LogisticRegressionCV(cv=10, random_state=0, max_iter=2000).fit(X, y)
print(clf_cv.scores_[1].mean())
clf_cv.scores_[1].shape


#%% cross validation
data_classify = data[(data['error.mon'] < 0.5) & (data['ambig_corr.mon.label'].notnull())]

y = data_classify['ambig_corr.mon.label']
X = data_classify.iloc[:,1:69]


n_iter = 50

score_cv = []
best10_coef = []
best10_name = []

for i in range(n_iter):
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.2)
    clf = LogisticRegression(max_iter=1000).fit(X_train, y_train)

    # sort and find 10 bset predictors
    coef_sorted = np.sort(clf.coef_)
    coef_idx = np.argsort(clf.coef_)

    ## what are the 10 best predictors
    best10_name.append(list(X_train.columns[coef_idx[0,0:9]]))
    best10_coef.append(list(coef_sorted[0,coef_idx[0,0:9]]))
    
    score_test = clf.score(X_test, y_test)
    print(score_test)
    score_cv.append(score_test)
    # y_pred = LogisticRegression(random_state=0,max_iter=1000).fit(X_train, y_train).predict(X_test)
    # print(classification_report(y_test, y_pred, target_names=['class0', 'class1']))

print('Average')
print(np.mean(score_cv))

#%% best predictors
# find the overlapping of best predictors across iterations
# .intersection, and .union work for set but not for list
# best10_name[0].intersection(best10_name[1], best10_name[2], best10_name[3],
#                             best10_name[4], best10_name[5], best10_name[6])

# best10_name[0].union(best10_name[9])

# find the predictors that appear over 70% of the best 10

counter_obj = Counter(chain.from_iterable(best10_name))
print(counter_obj.most_common())
print('Appear over: %s times' %(0.7*n_iter))

#%% ROC
# https://scikit-learn.org/stable/auto_examples/model_selection/plot_roc.html

# from sklearn.preprocessing import label_binarize

# Binarize the output

# y = label_binarize(y, classes=[0, 1]) # probably not necessary because my data is already binarized
# n_classes = y.shape[1]
data_classify = data[(data['error.mon'] < 0.5) & (data['ambig_corr.mon.label'].notnull())]

y = data_classify['ambig_corr.mon.label']
X = data_classify.iloc[:,1:69]

# shuffle and split training and test sets
# 50% for testing?
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.2,random_state=2)

# Predict confidence scores for samples.
# The confidence score for a sample is the signed distance of that sample to the hyperplane.
y_score = LogisticRegression(random_state=2,max_iter=1000).fit(X_train, y_train).decision_function(X_test)

# Compute ROC curve and ROC area for each class
fpr = dict()
tpr = dict()
roc_auc = dict()

fpr[1], tpr[1], _ = roc_curve(y_test, y_score)
roc_auc[1] = auc(fpr[1], tpr[1])

# Compute micro-average ROC curve and ROC areay
fpr["micro"], tpr["micro"], _ = roc_curve(y_test.ravel(), y_score.ravel())
roc_auc["micro"] = auc(fpr["micro"], tpr["micro"])

plt.figure()
lw = 2
plt.plot(fpr[1], tpr[1], color='darkorange',
         lw=lw, label='ROC curve (area = %0.2f)' % roc_auc[1])
plt.plot([0, 1], [0, 1], color='navy', lw=lw, linestyle='--')
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('Receiver operating characteristic, Monetary Ambiguity')
plt.legend(loc="lower right")
plt.show()

#%% ROC many iterations
data_classify = data[(data['error.mon'] < 0.5) & (data['ambig_corr.mon.label'].notnull())]

y = data_classify['ambig_corr.mon.label']
X = data_classify.iloc[:,1:69]

n_iter =50

auc_all = []

for i in range(n_iter):
    # shuffle and split training and test sets
    # 50% for testing?
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.2)
    
    # Predict confidence scores for samples.
    # The confidence score for a sample is the signed distance of that sample to the hyperplane.
    y_score = LogisticRegression(max_iter=1000).fit(X_train, y_train).decision_function(X_test)
    
    # Compute ROC curve and ROC area for each class
    fpr = dict()
    tpr = dict()
    roc_auc = dict()
    
    fpr[1], tpr[1], _ = roc_curve(y_test, y_score)
    roc_auc[1] = auc(fpr[1], tpr[1])
    
    auc_all.append(roc_auc[1])
    
    # Compute micro-average ROC curve and ROC areay
    fpr["micro"], tpr["micro"], _ = roc_curve(y_test.ravel(), y_score.ravel())
    roc_auc["micro"] = auc(fpr["micro"], tpr["micro"])
    
    plt.figure()
    lw = 2
    plt.plot(fpr[1], tpr[1], color='darkorange',
             lw=lw, label='ROC curve (area = %0.2f)' % roc_auc[1])
    plt.plot([0, 1], [0, 1], color='navy', lw=lw, linestyle='--')
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.05])
    plt.xlabel('False Positive Rate')
    plt.ylabel('True Positive Rate')
    plt.title('Receiver operating characteristic, Monetaru Ambiguity')
    plt.legend(loc="lower right")
    plt.show()

print('Mean area under curve:')
print(np.mean(auc_all))
#%%  classification report

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=.2,random_state=0)
y_pred = LogisticRegression(random_state=0,max_iter=1000).fit(X_train, y_train).predict(X_test)
print(classification_report(y_test, y_pred, target_names=['class0', 'class1']))


y_pred = LogisticRegressionCV(random_state=0,max_iter=1000).fit(X, y).predict(X)
print(classification_report(y, y_pred, target_names=['class0', 'class1']))

#%% Plot predictors distribution, for with or without errors
# 'actions_gro_hygiene', 'ambig1_news_sources_2', 'actions_leisureb4'
# predictor_name = 'actions_leisureb4'
# label_name = 'error.mon.label'

# 'actions_gro_hygiene', 'actions_leisureb4', 'ambig1_news_sources_3','ambig1_track_deaths'
predictor_name = 'ambig1_track_deaths'
label_name = 'error.med.label'

plt.figure()
plt.hist(data[predictor_name][data[label_name]==0], alpha=0.5)
plt.hist(data[predictor_name][data[label_name]==1], alpha=0.5)
plt.legend(['No error','With error'])
plt.title(predictor_name)

plt.show()
