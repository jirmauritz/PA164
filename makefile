EVALUATION=evaluation.R
NER=ner.R
BAGOW=bagow.R
SYNTAX=syntax.R

.PHONY: clean
all: results.csv


clean:
	@echo "### Cleaning ###"
	@rm -fr matrices
	@rm -f results.csv

# Bag of words
matrices/tf.bagow.csv: $(BAGOW)
	Rscript $(BAGOW)

matrices/tfidf.bagow.csv: matrices/tf.bagow.csv
	# do nothing
	noop



# process Named Entity Recognition
matrices/tf.ner.csv: $(NER)
	Rscript $(NER)

matrices/tfidf.ner.csv: matrices/tf.ner.csv
	#do nothing
	noop


# syntax
matrices/tf.subtrees.csv: $(SYNTAX)
	Rscript $(SYNTAX)


# evaluate
results.csv: matrices/tf.ner.csv matrices/tf.bagow.csv matrices/tf.subtrees.csv
	Rscript $(EVALUATION)
