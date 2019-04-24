rankhospital <- function(state, outcome, num = "best") {
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

## If the parameter num is not numeric and not one of the valid character values expected (best-worst) then stop with message
        if ( !is.numeric(num) && num != "best" && num != "worst" ) {
                stop ("invalid num")
        }

## Return hospital name in that state with the given rank
## 30-day death rate

## Identifie the rows that contains data of the parameter 'state' and charge it to the variable filtro_state
	data1 <- subset(data,State==state)

## Assign to the variable v_data the correct column of the data_frame 
	if ( outcome == "heart attack" ) {
## col11 Heart attack 30d mortality, as.numeric to force the interpretation as numeric
		v_data <- na.omit(data1[,c(1,2,11)])
        } else if ( outcome == "heart failure" ) { 
## col17 Heart failure 30d mortality, as.numeric to force the interpretation as numeric
			v_data <- na.omit(data1[,c(1,2,17)])
               } else if ( outcome == "pneumonia" )  {
## col23 Pneumonia 30d mortality, as.numeric to force the interpretation as numeric
				v_data <- na.omit(data1[,c(1,2,23)])
                      }

## Order by 3th column with the mortality rate selected by parameter outcome and in case of same value of outcome alphabetical
## The second parameter (hospital name) help to solve the case of tie, so we have it alphabetical as it is requires by the requirement
	names(v_data)[3] <- "valor_outcome"
	v_data2 <- v_data[order(v_data[,3], v_data[,2]),]
	filas <- nrow(v_data2)

## If the parameter num = best then return row = 1
	if ( num == 'best' ) {
		result <- as.character(v_data2[1,2])
## If the parameter num = worst then return the last row 
        } else if ( num == "worst" ) {
    		result <- as.character(v_data2[filas,2])
## If the parameter num is greater than the number of rows then return NA
        } else if ( num > filas ) {
		result <- NA
	} else {
## If the parameter num is a number then return then exact number row
    		result <- as.character(v_data2[num,2])
	}

	result
}

