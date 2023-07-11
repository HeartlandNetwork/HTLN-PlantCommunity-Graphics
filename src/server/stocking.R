stocking = function( dta, expfac=0, p=c(-0.00507,0.01698,0.00317), b=0.4 )
{
# Function to calculate the percent stocking my plot ot per acre
# by David R. Larsen, Copyright February 6, 2012
#
   sumD = sum( dta$dbh )
   sumD2 = sum ( dta$dbh^2 )

   if( expfac == 0 ) {
     tpa = length(dta$dbh)
     ba = sum(0.005454154*dta$dbh^2)
     dq = sqrt(sumD2/tpa)
   }else{ 
     tpa = length(dta$dbh) * expfac
     ba = sum(0.005454154*dta$dbh^2) * expfac
     dq = sqrt(sumD2*expfac/tpa)
     sumD = sumD * expfac
     sumD2 = sumD2 * expfac
   }
   sp = p[1]*tpa + p[2]*sumD + p[3]*ba/0.005454154
   sdi = tpa*(dq/10)^(1.605)
   rd = sum(ba)/(dq^b)
   data.frame(tpa=tpa,ba=ba,dq=dq, sp=sp, sdi=sdi, rd=rd )
} 
