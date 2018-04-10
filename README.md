Extend churn to forecast
Application of clustering to customer order data in three parts.  
First, a range of clustering methods including kmeans and hierarchal clustering are used to develop cluster models of customer data.  The factors include ordering patterns, customer longevity, recency of ordering, and other factors.
Next, the models are extended beyond a typical “churn” model by using the model in a cumulative fashion to predict customer re-ordering in the future defined by a set of time cutoffs.
Last, the cluster models are used to forecast actual revenue by estimating the ordering parameter distributions on a cluster basis, then sampling those distributions to predict new orders and order values over a time interval.
