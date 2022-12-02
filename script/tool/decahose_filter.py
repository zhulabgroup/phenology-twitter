import os
import pyspark.sql.functions as F

from argparse import ArgumentParser
from pyspark.sql.functions import udf
from pyspark.sql.types import BooleanType

from pyspark import SparkContext, SparkConf
from pyspark.sql import SQLContext

conf = SparkConf()
sc = SparkContext(conf=conf)
sqlContext = SQLContext(sc)


#Get the command-line arguments.
parser = ArgumentParser()
parser.add_argument('-k', '--keyword-file', required=True, help='Path to the local file containing the list of keywords.')
parser.add_argument('-i', '--input', required=True, help='Path to the input (file or directory) in HDFS. May also use file:// for local file.')
parser.add_argument('-o', '--output', required=True, help='HDFS output directory path.')
args = parser.parse_args()

keywordFile = args.keyword_file
inputFile   = args.input
outDir      = args.output

#Load the keywords to use from filtering.
keywords = set()
for line in open(keywordFile, 'r'):
	keywords.add(line.strip().lower())


#Custom UDF for filtering a text column. Returns true if the column is non-null
#and at least one keyword is contained in the text.
def keyword_match(text):
	return text != None and any(keyword in text for keyword in keywords)
udf_keyword_match = udf(keyword_match, BooleanType())


#Load the data
df = sqlContext.read.json(inputFile)

#Keep data where text or extended tweet text matches the keywords
filtered = df.where( (udf_keyword_match(F.lower(F.col('text')))) 
	| ( (df.truncated == True) 
		& ( udf_keyword_match(F.lower(F.col('extended_tweet.full_text'))) )
		) 
	)

#Write the filtered data to the output path.
filtered.write.json(os.path.join(outDir))
