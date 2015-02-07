EVALUATION=evaluation.R
NER=ner.R
BAGOW=bagow.R

.PHONY: clean
all: results.csv


clean:
	@echo "### Cleaning ###"
	@rm -fr matrices
	@rm -f results.csv

# Matrices
matrices/tf.Matrix.csv matrices/tfidf.Matrix.csv: ner.matrices.intermediate

# Bag of words
matrices/bagow.csv: $(BAGOW)
	Rscript $(BAGOW)

# process Named Entity Recognition
.INTERMEDIATE: ner.matrices.intermediate
ner.matrices.intermediate: $(NER)
	Rscript $(NER)

# evaluate
results.csv: ner.matrices.intermediate matrices/bagow.csv
	Rscript $(EVALUATION)
