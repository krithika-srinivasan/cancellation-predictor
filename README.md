# cancellation-predictor
A model that uses BERT transformers and corresponding preprocessing layer that predicts whether a given tweet is "cancelling" someone (0), supporting/defending them (2) or is engaging with the cancelling in some way but is not cancelling or defending them, such as jokes, expressly non-committal statements, etc (1)

The training dataset is based on the Lindsay Ellis Cancellation, with tweets manually annotated as critical, supportive or neutral. 

[Write Up About The Cancellation](https://krithikasrinivasans.medium.com/i-read-20-000-tweets-from-the-lindsay-ellis-cancellation-and-heres-what-i-learned-eef69894c15b)

[How I Made The Dataset](https://www.tensorflow.org/tutorials/structured_data/feature_columns) 

[Source for the model] (https://www.tensorflow.org/text/tutorials/classify_text_with_bert)
