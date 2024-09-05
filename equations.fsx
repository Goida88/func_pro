let epsilon = 0.001

let g1 x = cos(x) - exp(-(x**2.)/2.) + x - 1.
let g2 x = 1. - x + sin(x) - log(1. + x)
let g3 x = 3. * x - 14. + exp(x) - exp(-x)

let dg1 x = x * exp(-x**2./2.) - sin(x) + 1.
let dg2 x = cos(x) - 1. - 1./(x + 1.)
let dg3 x = exp(x) + 3. + exp(-x)

let psi1 x = -cos(x) + exp(-(x**2.)/2.) + 1.
let psi2 x = 1. + sin(x) - log(1. + x)
let psi3 x = log(-(3. * x - 14. - exp(-x)))

let signum a = if a > 0. then 1. else if a < 0. then -1. else 0.

let rec dichotomy g a b = 
  if signum(g a) = signum(g b) then
    b
  else
    let midpoint = (a + b) / 2. 
    if g midpoint = 0. || (midpoint - a) < epsilon then
      midpoint
    else if ((g a) * (g midpoint) < 0.) then
      dichotomy g a midpoint
    else
      dichotomy g midpoint b

let iterative g a b = 
  let rec loop g current_x = 
    let next_x = g current_x
    if abs(next_x - current_x) < epsilon then
      next_x
    else
      loop g next_x
  
  let initial_x = (a + b) / 2.
  loop g initial_x

let newton g dg a b =
  let rec loop g dg current_x = 
    let next_x = current_x - (g current_x) / (dg current_x)
    if abs(next_x - current_x) < epsilon then next_x
    else 
      loop g dg next_x
      
  let initial_x = (a + b) / 2.
  loop g dg initial_x
  
let run = 
  printfn "---------------------------------------------------------------------------"
  printfn "|    f    |  a  |  b  |   Dichotomy  |    Iterative   |    Newton    |  Expected  |"
  printfn "---------------------------------------------------------------------------"
  printfn "|    1    |  1  |  2  |  %10.6f|  %10.6f|  %10.6f|  %10.6f|" (dichotomy g1 1. 2.) (iterative psi1 1. 2.) (newton g1 dg1 1. 2.) 1.0804
  printfn "---------------------------------------------------------------------------" 
  printfn "|    2    |  1  | 1.5 |  %10.6f|  %10.6f|  %10.6f|  %10.6f|" (dichotomy g2 1. 1.5) (iterative psi2 1. 1.5) (newton g2 dg2 1. 1.5) 1.1474
  printfn "---------------------------------------------------------------------------"
  printfn "|    3    |  1  |  3  |  %10.6f|  %10.6f|  %10.6f|  %10.6f|" (dichotomy g3 1. 3.) (iterative psi3 1. 3.) (newton g3 dg3 1. 3.) 2.0692
  printfn "---------------------------------------------------------------------------"

run
