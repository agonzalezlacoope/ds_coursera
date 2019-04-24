best <- function(state, outcome) {
## Read outcome data
## Charge in variable data all the lines of the file, identifying as NA the string "Not Available"
	data <- read.csv('outcome-of-care-measures.csv', na.strings="Not Available")

## Check that state and outcome are valid
## If length of vector filtered by parameter passed state is 0 stop error
## Because the parameter 'state' is not in the valid states existing in data frame
	v_states <- data[, 'State']
	if ( length(v_states[v_states==state]) == 0 ) {
		stop ("invalid state")
        }

## If length of vector filtered by parameter passed outcome is 0 stop error
## Because then parameter 'outcome' is not one of the three valid values expected
	v_outcomes <- c('heart attack','heart failure','pneumonia')
	if ( sum(v_outcomes==outcome) == 0 ) {
		stop ("invalid outcome")
        }

## Return hospital name in that state with lowest 30-day death rate

## Identifie the rows that contains data of the parameter 'state' and charge it to the variable filtro_state
	filtro_state <- which(v_states==state)
## Assign to the variable v_hospitales the names of the hospitals of the state filtered
	v_hospitales <- data[filtro_state,2]	
## Assign to the variable v_data the correct column of the data_frame 
	if ( outcome == "heart attack" ) {
## col11 Heart attack 30d mortality, as.numeric to force the interpretation as numeric
		v_data <- as.numeric(data[filtro_state,11])	
        } else if ( outcome == "heart failure" ) { 
## col17 Heart failure 30d mortality, as.numeric to force the interpretation as numeric
			v_data <- as.numeric(data[filtro_state,17])	
               } else if ( outcome == "pneumonia" )  {
## col23 Pneumonia 30d mortality, as.numeric to force the interpretation as numeric
				v_data <- as.numeric(data[filtro_state,23])	
                      }

## Identifie the minimun value of the mortality for the filter assigned and discarding the NA values
  	min_valor <- min(v_data, na.rm=TRUE)
## Assign to filas the rows with the minimun value identified
  	filas <- which(v_data==min_valor)

## If we have more than one hospital, we sort alphabetical an then return the first row
  	if ( length(filas) > 1 ) {
		sort_name <- sort(v_hospitales[filas])
  		result <- (sort_name[1])
## If we have only one row return the name of the hospital of this row
        } else {
    		result <- (v_hospitales[filas])
        }

	result

}

