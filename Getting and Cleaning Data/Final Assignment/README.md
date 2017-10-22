## Included Files
### Subfolder "Data"
This folder contains the train and test data in seperate folders. Additionally, it contains information about how the data was obtained and preprocessed. From a point of view of this analysis, it contains raw data.
Information can be acquired using the included documentation files.
### run_analysis.R
This script performs the main analysis. In particular, the following steps are executed:
* The test and train data is loaded into variables. Since the data is split into subject_train.txt (containing the subject ID per observation), x_train.txt (containing the measurements per observation) and y_train.txt (containing the activity ID per observation) (and _test, respectively), first a colbind is performed and afterwards a rowbind to generate a single large dataframe.
* The feature names are extracted from "data/features.txt", allowing the selection of a subset of the columns.
* The selected subset only contains variables with "mean" or "std" in their name, except for mean frequencies ("meanfreq"). 
* Feature names were replaced by names with improved readability. See the codebook for further information about name changes.
* The activity ID was replaced by a verbal description of the activity.
* The data was grouped by activity and subject and the mean value per group was computed.
* This data was saved as a CSV file without headers, so an accompanying feature_names.txt file has been generated as well.