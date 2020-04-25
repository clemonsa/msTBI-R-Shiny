# Predicting Seizure Events Following Moderate-to-Severe Traumatic Brain Injury
***Updated***: *4.23.2020*  
**PTS** - Post-Traumatic Seizures  
**TBI** - Traumatic Brain Injury

This is a preliminary application developed from work conducted under Dr. Amy Wagner from the University of Pittsburgh, developing a grouped LASSO logistic regression prediction model of post-traumatic seizure events. The application is currently in its early stages, with a number of potential improvements planned, including but not limited to the brief list below:

*N.B.* This application was preliminary developed as a course project in the University of Pittsburgh Graduate School of Public Health's Department of Biostatistics course: BIOST2094 Advanced R Computing. This work was developed by Arvon Clemens II, Felix Proessl, and Dominic DiSanto. 

Following completion of the course (4/14/2020), the application has continued to undergo development under supervision of Dr. Amy Wagner, is currently mainted by Dominic DiSanto.


**Pending/Potential Additions & Changes**
Update this README to be more useful, comprehensive
Adding year 1 model page/tab
Adding acute seizureless model page/tab
Adding PPV, NPV to performance
Separating model performance data table from patient output table 
Look into other plotting packages to allow for more effiecient, reactive plotting if possible (plotly object seems slow to render)
Make plotly hoverover text include additional information, namely percentiles
Include more explicit definitions for all abbreviations and variables, including information relevant to data source/collection of information 
   e.g. 
  "acute seizures=electroencephalographic or symptomatic evidence of a  
	seizure during acute care hospitalization", "neurodegenerative disease 
  may be derived from ICD diagnosis codes or taken from 
	medical records or patient reported history", etc. 

Exportable spreadsheet option to store results in a document that individual physicians can use to track patients, inputs into app
	Including a blank/free text entry field for "Patient ID" spot when a "save record" box is checked
