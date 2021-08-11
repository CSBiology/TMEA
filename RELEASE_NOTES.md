#### 0.5.0 - Friday, June 18, 2021
- Add more customization options to potential time course and heatmap plots
- Improve implementation of optional arguments for plot functions by returning a TMEAResult transforming funktion instead of the plot functions taking the result directly as input
- Rework plot module:
	- optional parameter passing style for all plot functions
	- Rename: 
		- generateConstraintTimeCoursePlot -> generateConstraintPotentialTimeCoursePlot
		- plotConstraintTimeCourses -> plotConstraintPotentialTimeCourses
		- generatePotentialHeatmap -> generateConstraintPotentialHeatmap
		- plotPotentialHeatmap -> plotConstraintPotentialHeatmap
- add `readDataFrameFromString` function

#### 0.4.0 - Monday, June 7, 2021
- Add functionality to invert constraints and respective potentials for a given TMEAResult

#### 0.3.0 - Thursday, June 3, 2021
- Support net5.0
- Update plotly dependencies
- Add `calculate_TotalFreeEnergyTimeCourse` func

#### 0.2.1 - Thursday, May 25, 2021
- fix a bug that only allows tab separated data frame input

#### 0.2.0 - Monday, May 19, 2021
- target netstandard 2.0 for maximum compatibility for now

#### 0.1.1 - Monday, October 19, 2020
- added read function for ontology maps
- added read functions from stream for data frames and ontology maps

#### 0.1.0 - Tuesday, September 8, 2020
- added Surprisal Analysis cp Heatmap Plot
- added Surprisal Analysis analytic plots: 
  - Constraint importance
  - Gradual data recovery
- major namespace simplification
- added analysis pipeline functions
- added FAS weight distribution plot

#### 0.0.2-alpha - Wednesday, September 2, 2020
- added Plot functions for Surprisal Analysis results.
- fixed Authors of the nuget package

#### 0.0.1-alpha - Sunday, August 17, 2020
- added the necessary function to run the basic pipeline, consisting of applying surprisal analysis to the dataset and subsequently applying our monte carlo sampling test to biologically characterize the results.