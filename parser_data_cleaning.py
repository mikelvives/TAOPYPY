# -*- coding: utf-8 -*-
import importlib
import pandas as pd
from pattern.search import search
import re
import string
import unidecode

countries = ['ES', 'IT', 'FR', 'MX']
exceptions = ['mal', 'bad', 'no', 'si']

def get_value_counts(listwords):
	'A function to get a count of the amount of times a word is repeating inside a list'
	d = dict.fromkeys(listwords, 0)
	for elem in listwords:
		d[elem] += 1

	return d

def remove_trash(listwords):
	'A function to remove all unwanted results from a dictionary of word counts'
	# Type of expressions that we want to be removed:
	exp1 = re.compile(r'[(\+\d{2})\d{4}]?[-\.]?\d{3}[-\.]?[\d{3}\d{2}][-\.]?[\d{3}\d{2}][-\.]?\d{2}?')
	exp2 = re.compile(r'(?i)wh?at?sap?p?')
	exp3 = re.compile(r'\s(\w{0,3})\s')
	
	# Remove keys in the dictionary that match any of the three expressions above
	for elem in listwords:
		aux = ' ' + elem + ' '
		if bool(re.search(exp1, aux)) or bool(re.search(exp2, aux)) or bool(re.search(exp3, aux)):
			if elem.lower() not in exceptions:
				listwords.remove(str(elem))
	return listwords

def parseNLP(s, lang):
	'A function that will parse a text. Will return all the nouns in it in singular, as well as all the verbs in infinitive'
	
	# First, check that the given parameters are trully strings
	try:
		assert type(s) == str or type(s) == unicode
	except:
		return {'adv' : {}, 'nouns' : {}, 'adj' : {}}

	try:
		assert type(lang) == str or type(lang) == unicode
	except:
		return {'adv' : {}, 'nouns' : {},'adj' : {}}

	# Get the correct module for the selected language from the pattern package 
	if lang.lower() == 'mx':
		lang = 'es'
	if lang.lower() == 'ch':
		lang = 'fr'
	try:
		m = importlib.import_module('pattern.'+lang.lower())
	except Exception as e:
		print(e)
		return {'verbs' : {}, 'nouns' : {}}
	# Strip accents and add spaces between punctuation signs
	if (type(s) != unicode):
		s = unicode(s, "utf-8")
		
	s = unidecode.unidecode(s)

	s = re.sub('\x00', ' ', s)
	s = re.sub('#', '', s)
	s = re.sub(r'@\w*', '', s)
	#s = re.sub('(?<! )(?=[^\w])|(?<=[^\w])(?! )', r' ', s)
	s = re.sub('\s{2,}', ' ', s)
	

	# Grab all nouns and verbs
	pt = m.parsetree(s, lemmata = True)
	nouns_pre = [elem.words for elem in search('NN*', pt)]
	verbs_pre = [elem.words for elem in search('VB*', pt)]
	adj = [elem.words for elem in search('JJ*', pt)]
	adv = [elem.words for elem in search('RB*', pt)]

	nouns_post = []
	adj_post = []
	adv_post = []

	for elem in nouns_pre:
		nouns_post.extend([word.lemma for word in elem])
	for elem in adj:
		adj_post.extend([word.lemma for word in elem])
	for elem in adv:
		adv_post.extend([word.lemma for word in elem])

	verbs_post = []
	for elem in verbs_pre:
		verbs_post.extend([word.lemma for word in elem])

	s = ', '.join(remove_trash(nouns_post)) + '. ' + ', '.join(remove_trash(verbs_post))+ '. ' + ', '.join(remove_trash(adv_post)) + '. ' + ', '.join(remove_trash(adj_post))
	# Return the count of each word
	#d = {'nouns' : remove_trash(nouns_post), 'adv': remove_trash(adv_post), 'adj' : remove_trash(adj_post), 'verbs' : remove_trash(verbs_post)}
	return s



'''if __name__ == '__main__':
	aux = pd.DataFrame(columns = ['description', 'offerid', 'parsetree', 'nouns', 'verbs', 'country'])

	for country in countries:
		print('Checking data for country: ' + country)
		df = pd.read_csv('sample_' + country + '.csv', sep = ';', encoding = 'utf-8')
		df['description'] = df.apply(lambda x: x.description, axis = 1)
		df['nouns'] = df.apply(lambda x: parseNLP(x.description, country)['nouns'], axis = 1)
		df['verbs'] = df.apply(lambda x: parseNLP(x.description, country)['verbs'], axis = 1)
		df['country'] = pd.Series([country]* df.shape[0])
		aux = aux.append(df)
		print('Done with the country: ' + country)

	aux.to_excel('results.xlsx')
'''
		