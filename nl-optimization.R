library('nloptr')
## f(x) = x1*x4*(x1 + x2 + x3) + x3#
eval_f <- function( x ) {
  return( list( "objective" = x[1]*x[4]*(x[1] + x[2] + x[3]) + x[3] )) 
}#,#"gradient" = c( x[1] * x[4] + x[4] * (x[1] + x[2] + x[3]),x[1] * x[4],x[1] * x[4] + 1.0,x[1] * (x[1] + x[2] + x[3]) ) ) )}

# constraint functions
# inequalities
eval_g_ineq <- function( x ) {
  constr <- c( 25 - x[1] * x[2] * x[3] * x[4] )
  #grad   <- c( -x[2]*x[3]*x[4],-x[1]*x[3]*x[4],-x[1]*x[2]*x[4],-x[1]*x[2]*x[3] )
  return( list( "constraints"=constr)) #, "jacobian"=grad ) )
  }

# equalities
eval_g_eq <- function( x ) {
  constr <- c( x[1]^2 + x[2]^2 + x[3]^2 + x[4]^2 - 40 )
  #grad   <- c(  2.0*x[1],2.0*x[2],2.0*x[3],2.0*x[4] )
  return( list( "constraints"=constr)) #, "jacobian"=grad ) )
  }

# initial values
x0 <- c( 1, 5, 5, 1 )

# lower and upper bounds of control
lb <- c( 1, 1, 1, 1 )
ub <- c( 5, 5, 5, 5 )
local_opts <- list( "algorithm" = "NLOPT_LD_MMA","xtol_rel"  = 1.0e-7 )
opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
              "xtol_rel"  = 1.0e-7,
              "maxeval"   = 1000,
              "local_opts" = local_opts )
res = nloptr( x0=x0,
              eval_f=eval_f,
              lb=lb,
              ub=ub,
              eval_g_ineq=eval_g_ineq,
              eval_g_eq=eval_g_eq,
              opts=opts)
res              
?nloptr
