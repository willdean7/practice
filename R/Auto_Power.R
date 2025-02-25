#
#' Writing a Function for Automobile Power Generation
#' @param V is vehicle speed (assuming no headwind) in m/s
#' @param m is vehicle mass in kg
#' @param A is the surfance of the car in m^2
#' @param g is acceleration due to gravity (9.8 m/s^2)
#' @param p_air is the density of air
#' @param Pb is power in watts
#' @param crolling is the coefficient of rolling resistance
#' @param cdrag is the coefficient of drag
#' @return Pb



auto_power <- function(V, m, A, g = 9.8, p_air=1.2, crolling = 0.015, cdrag = 0.3){
  V=ifelse((V <0), return("Speed cannot be negative"), V)
  m= ifelse((m <0), NA, m)
  A= ifelse((A <0), NA, A)
  Pb <- (0.5 * p_air * A * cdrag * V^3) + (crolling * m * g * V)
  return(Pb)
}

auto_power(30, 31752, 16)
# 217786.3
auto_power(c(5,10,15), 10000, 20)

kmh_to_ms <- function(kmh){
  return(kmh * 1000 / 3600)
}

auto_power(kmh_to_ms(80), 31752, 16)
# 135328.1
auto_power(kmh_to_ms(120), 31752, 16)
# 262251.5
auto_power(kmh_to_ms(40), 31752, 16)
# 55812.22
auto_power(kmh_to_ms(5), 31752, 16)
# 6490.416

#plot power as a function of speed
speeds <- seq(0, 120, 1)
powers <- sapply(speeds, function(x) auto_power(kmh_to_ms(x), 31752, 16))
plot(speeds, powers, type = "l", xlab = "Speed (km/h)", ylab = "Power (W)", main = "Power as a function of speed")

#recompute for lighter vehicle (15000 kg)
light_powers <- sapply(speeds, function(x) auto_power(kmh_to_ms(x), 15000, 16))
lines(speeds, light_powers, col = "red")

