import pandas as pd
import sys
reload(sys)
sys.setdefaultencoding('UTF8')
import parser_data_cleaning as p

from sklearn.feature_extraction.text import CountVectorizer



if __name__ == '__main__':
	df = pd.read_csv('tweets_public.csv', index_col='tweet_id')
	df['words_parsed'] = df.apply(lambda row: p.parseNLP(row.text, 'EN'), axis = 1)

