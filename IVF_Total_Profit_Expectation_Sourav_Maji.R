#============================================================================================================
# An IVF Hospital Chain with a nationwide capacity of 1000 patients per month and 100 patients per center 
# have been instructed to do annual budget planning. As part of the same, they are analyzing patient footfalls
# and revenues.The chain follows a package pricing system. They makes $1000 profit per baby born,
# $200 for a twin, $-1000 for a triplet and $-2000 when it is a quadruplet.
# It makes no profit when there is a miscarriage.
# The average footfall per month follows an uniform distribution between 72 to 84 per center. 
# Analyzing the data from the past, you found there is a 25% chance of twin, 5% chance for a triplet, 
# 1% chance of a quadruplet and 4% chance of miscarriage.
# Given the CEO wants a detailed profit projection, can you help the IVF chain?
#==========================================================================================================
#1. LOGIC
#==========================================================================================================
# a)Given total capacity and patients per center,so computed total hospitals
# b)Required to compute Profit for the whole year
# c)Computed profit with number of babies count with package on number of babies born
# d)Used two for loop one for the number of months and one for no of centers
# e)Used a UDF to compute the algorithm to get every hospitals profit per month and appended to the dataframe
# f)After all the compute we got the expected profit for the IVF Hospital Chain
#==========================================================================================================
# 2.Data Containers
#==========================================================================================================
# a) Data frames
# b) Vectors
#==========================================================================================================
# 3.Functions
#==========================================================================================================
# a) for loop one for month one for total hospitals
# b) runif to get uniform number between min an max number of patients
# c) cbind to column bind the value to fit into dataframe
# d) rbind to stack values received from each iteration
# e) data.frame to store the end results
# f) sum to get sum of each columns
# g) A user defined function to get the result
#==========================================================================================================
# 4. Procedure
#==========================================================================================================

#User defined function
profit_calculation = function(total_center,c_max,c_min,c_package_names,c_package_prob,c_package)
{
        #Dataframe to store the total data
        total_profit =data.frame()
        #Loop for months
        for (i in 1:12)
                {
                #Loop for the number of hospitals
                for (j in 1:total_center)
                        {
                        #getting the random number using random uniform
                        patient_count = round(runif(1,min = c_min,max=c_max),0)
                        #getting the sample of babies born in month with babies count from patient count
                        #probabilites of each types of babies are taken from past data
                        babies = sample(x=c_package_names,size=patient_count,replace = T,
                                        prob = c_package_prob)
                        #sum of the each type of babies birth count are taken here
                        babies_count =cbind(Single = length(babies[babies=="Single"]),
                                        Twin = length(babies[babies=="Twin"]),
                                        Triplet = length(babies[babies=="Triplet"]),
                                        Quadruplet = length(babies[babies=="Quadruplet"]),
                                        Miscarriage = length(babies[babies=="Miscarriage"]))
                        #Total profit/loss is take by multiplying babies count with
                        #the package profit/loss to the hospital
                        c_total_profit = sum(babies_count*c_package)
                        #Getting all the data together 1.Hospital_no 2. babies_born_type
                        #3.The calculate profit/loss
                        all_center = cbind(j,babies_count,c_total_profit)
                        #Creating the dataframe adding the month column with the previous data
                        last_df = data.frame(i,all_center)
                        #Stacking the data for each loop
                        total_profit = rbind(total_profit,last_df)
                        }
                }
        #Creating the last row with the total number of
        # 1.Months 2.Hospital_No 3.Types of Babies_born 4.Total Profit
        total_sum = c(12,10,sum(total_profit$Single),sum(total_profit$Twin),sum(total_profit$Triplet),
                sum(total_profit$Quadruplet),sum(total_profit$Miscarriage),sum(total_profit$c_total_profit))
        #Adding the last row with total to the final dataframe
        total_profit = rbind(total_profit,total_sum)
        #Naming the columns
        names(total_profit) = c("Month","Hospital_no","Single","Twin","Triplet","Quadruplet","Miscarriage","Total_Profit")
        return(total_profit)
}
#==========================================================================================================
# 5. Initializing the data from the question
#==========================================================================================================
n_capacity = 1000
c_capacity = 100
total_center = n_capacity/c_capacity
c_package = c(1000,200,-1000,-2000,0)
c_package_names = c("Single","Twin","Triplet","Quadruplet","Miscarriage")
c_package_prob = c(0.65,0.25,0.05,0.01,0.04)
c_min = 72
c_max = 82
#===========================================================================================================
# 6.Computing the Userdefine function to get the result
#===========================================================================================================
#Calling the UDF
IVF_total_profit = profit_calculation(total_center,c_max,c_min,c_package_names,c_package_prob,c_package)
#Printing the result
print(IVF_total_profit)
#Viewing the total data
View(IVF_total_profit)

#Getting the estimate total profit
print(paste("Expected profit of this year for IVF Hospitals is :",
            IVF_total_profit[nrow(IVF_total_profit),ncol((IVF_total_profit))],sep = " "))
#===========================================================================================================
