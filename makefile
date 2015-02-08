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
matrices/bagow.csv: $(BAGOW)
	Rscript $(BAGOW)



# process Named Entity Recognition
matrices/tf.matrix.counts.csv: $(NER)
	Rscript $(NER)

matrices/tfidf.matrix.counts.csv: matrices/tf.matrix.counts.csv
	#do nothing
	noop


# syntax
matrices/tf.matrix.subtrees.csv: $(SYNTAX)
	Rscript $(SYNTAX)


# evaluate
results.csv: matrices/tf.matrix.counts.csv matrices/tfidf.matrix.counts.csv matrices/bagow.csv matrices/tf.matrix.subtrees.csv
	Rscript $(EVALUATION)
