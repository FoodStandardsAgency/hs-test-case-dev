import spacy

#nlp = spacy.load("python/model/en_core_web_sm/en_core_web_sm-2.2.5")
#nlp = spacy.load("en_core_web_sm")


#def get_lemmas(wordlist):
#  """Takes a list of words, parses and tags them, and returns a list of corresponding lemmas"""
#  lemmas = []
#  for word in wordlist:
#    doc = nlp(word)
#    for token in doc:
#      lemmas.append([token.lemma_])
#  return lemmas
  
#def get_token_attributes(string):
#  """Takes a string, tokenises, parses and tags it, and returns the attributes of each token"""
#  tokens = []
#  doc = nlp(string)
#  for token in doc:
#    tokens.append([token.text, token.lemma_, token.pos_, token.tag_, token.dep_,
#                  token.shape_, token.is_alpha, token.is_stop])
#  return tokens
