rankall <- function(outcome, num = "best") {
## Read outcome data
## Charge in variable data all the lines of the file, identifying as NA the string "Not Available"
	data <- read.csv('outcome-of-care-measures.csv', na.strings="Not Available")

## Check that state and outcome are valid

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

## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name

## Assign to a vector all the states in the data.frame
	states <- unique(data[,'State'])
	order_states <- states[order(states)]

## Begin with an empty data.frame
	result <- data.frame()

## Make a loop to rank each state and assign the result to a data.frame
	for ( state in order_states ) {
## Take the subset of hospitals of the given state
        	data1 <- subset(data,State==state)

## Assign to the variable v_data the correct column of the data_frame 
		if ( outcome == "heart attack" ) {
## col11 Heart attack 30d mortality, as.numeric to force the interpretation as numeric
			v_data <- na.omit(data1[,c(2,7,11)])
        	} else if ( outcome == "heart failure" ) { 
## col17 Heart failure 30d mortality, as.numeric to force the interpretation as numeric
			v_data <- na.omit(data1[,c(2,7,17)])
               	} else if ( outcome == "pneumonia" )  {
## col23 Pneumonia 30d mortality, as.numeric to force the interpretation as numeric
			v_data <- na.omit(data1[,c(2,7,23)])
                }

## Order by 3th column with the mortality rate selected by parameter outcome and in case of same value of outcome alphabetical
## The second parameter (hospital name) help to solve the case of tie, so we have it alphabetical as it is requires by the requirement
		names(v_data)[1] <- "HospName"
		names(v_data)[2] <- "State"
		names(v_data)[3] <- "valor_outcome"
		v_data2 <- v_data[order(v_data[,3], v_data[,1]),]
		filas <- nrow(v_data2)


## Pick the correct choice depending on parameter num and assign to a data.frame result1
## If the parameter num = best then return row = 1
		if ( num == 'best' ) {
			result1 <- data.frame('hospital'=v_data2[1,1], 'state'=v_data2[1,2])
## If the parameter num = worst then return the last row 
        	} else if ( num == "worst" ) {
    			result1 <- data.frame('hospital'=v_data2[filas,1], 'state'=v_data2[filas,2])
## If the parameter num is greater than the number of rows then return NA
        	} else if ( num > filas ) {
			result1 <- data.frame('hospital'=NA, 'state'=state)
		} else {
## If the parameter num is a number then return then exact number row
    			result1 <- data.frame('hospital'=v_data2[num,1], 'state'=v_data2[num,2])
		}
## Add to the resul data.frame to collect all the data 
		result <- rbind(result, result1)
	}

	result
}

