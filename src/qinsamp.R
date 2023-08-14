##     qinsamp.R: R script calculating sample quatiles.
##     Copyright (C) 2012-2016 Matthew T. Pratola, Robert E. McCulloch and Hugh A. Chipman
##
##     This file is part of RBART
##
##     RBART is free software: you can redistribute it and/or modify
##     it under the terms of the GNU Affero General Public License as published by
##     the Free Software Foundation, either version 3 of the License, or
##     (at your option) any later version.
##
##     RBART is distributed in the hope that it will be useful,
##     but WITHOUT ANY WARRANTY; without even the implied warranty of
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##     GNU Affero General Public License for more details.
##
##     You should have received a copy of the GNU Affero General Public License
##     along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
##     Author contact information
##     Matthew T. Pratola: mpratola@gmail.com
##     Robert E. McCulloch: robert.e.mculloch@gmail.com
##     Hugh A. Chipman: hughchipman@gmail.com


######################################################################
qinsamp = function(y,ysamp) { ###get quantile of y in sample ysamp
n=length(ysamp)
return(which.min(abs(y-sort(ysamp)))/n)
}
qsamp = function(y,yd) {
nd=nrow(yd)
n=ncol(yd)
qvec=rep(0,n)
for(i in 1:n) {
   qvec[i]=qinsamp(y[i],yd[,i])
}
return(qvec)
}

