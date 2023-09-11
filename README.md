# Statisitcal-Analysis-of-Indian-Health-Database-in-R

The task of this part is to analyze the following points using a simulation:
- the application of central limit theorem in statistical analysis
- to investigate the limitations of the assumptions for a parameteric test
- analyse the alternatives if the assumptions are violated.
In the simulation you can control the population parameters. Based on that you
can simulate experiments that sample from that population and you can analyse if the
conclusion based on the simulated samples are correct.
Do the following steps:
- Generate a population with 1 000 000 subjects using poisson distribution with the
parameter λ = 2 .
- Take n number of samples where n is much larger than 30 and conduct a t-test with a
reference value where the nullhypothesis is true. Show that the α−value represents
the type-I error by repeating the experiment multiple times and evaluating the
number how often the Nullhypothesis is kept and rejected.
- Show that the t-test is not functioning correctly when the sample number becomes
small. Which assumptions are violated?
- Show that in case the sample size become too small, an alternative test can work.
(Hint: Use high quality graphics to explain and illustrate you results)

Data analysis-1: 
Conduct the following tasks:
1. Import the population data from the excel sheet "India_Health_Database". To obtain
   population data for the years 2012-2017, extrapolate the total population for each
   state and each year, with the data from 2011 and 2018 assuming a linear increase or
   decrease.
2. Take the dataset of Dengue and compute the lethality for corresponding years and
   state. What is the average lethality of all states, and what are the values for the
   confidence interval +/- 1 SD? Compare your values to other resources using other
   references. Does your confidence interval include this value?
3. Is there a significant difference in the lethality for the different years? Formulate
   a null hypothesis, apply the correct statistical test and analyse the results.(Be
   careful with small case numbers and death numbers)
4. Does the lethality correlate with the urbanisation level? Explain your results.

Data analysis-2:
1. In the attached excel sheet "Glucose_BP_levels" is a dataset of glucose levels and the
   systolics blood pressure. The resarch question if there is a relation between glucose
   levels and high blood pressure. Choose the correct statistical analysis method and
   evaluate if there is a measurable effect. Please consider the following points:
   - Can we predict values beyond the given datapoints from the dataset? or in other words
     is the model valid for any value of x beyond the dataset?
   - Compute the Confidence intervals with α = 1 % and include them in the graphical representation
   - Analyse the residuals of your model. What do you observe? Discuss the results and potential
     fallacies.
