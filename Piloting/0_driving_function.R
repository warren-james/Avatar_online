# function to drive the bot
bot_drive <- function(A_pos, T_pos, bot_pars, reach, sp){
  x_pos = A_pos
  y_pos = 100
  x_targ = T_pos
  y_targ = 0
  
  # setup delays 
  Rt = bot_pars$Rt
  delay = bot_pars$delay
  down_travel = bot_pars$down_travel
  # Rt = round(rlnorm(1, bot_pars$RT_mu, bot_pars$RT_sd))
  # delay = round(rlnorm(1, bot_pars$switch_delay_mu, bot_pars$switch_delay_sd))
  # 
  # # setup stopping point 
  # down_travel = round(rnorm(1, bot_pars$down_stop_mu, bot_pars$down_stop_sd))
  
  # hit detection
  hit = NA
  suc = 0
  
  # motion 
  cur_mot = ""
  prev_mot = ""
  
  # things to check 
  driving = F
  down_reached = c(F, 0)
  delay_count = 0
  overshoot = F # do we need this?
  switching = F
  
  for(t in 1:120){
    # Are we driving
    if(t >= Rt & switching == F){
      driving = T
    }
    
    # have we finished waiting 
    if(delay_count == delay){
      switching = F
      driving = T
      delay_count = 0
    }
    
    # check if we've gone too far 
    if(x_pos > x_targ + reach*.5 + sp & down_reached[1] == T){
      overshoot = T
      down_reached[2] = 1
    }
    
    if(y_pos <= 100 - down_travel + sp & down_reached[2] == 0){
      down_reached[1] = T
      down_reached[2] = 1
      switching = T
    }
    
    if(switching){
      delay_count = delay_count + 1
      driving = F
      prev_mot = ""
      # cur_motion = ""
    }
    
    # how are we moving this frame 
    if(driving){
      # move down if we haven't got low enough
      # or 
      # we've gone too far to the side
      if(down_reached[1] == F | overshoot == T){
        y_pos = y_pos - sp
        cur_mot = "down"
        # otherwise, got side ways
      } else {
        x_pos = x_pos + sp
        cur_mot = "side"
      }
    }
    
    # check, have we switched motion 
    if(cur_mot != prev_mot & length(prev_mot) > 1){
      switching = T
    } 
    
    prev_mot = cur_mot
    
    # check if we're in range... if so, quit
    hit = ellipse_dist(x_pos, y_pos, x_targ, y_targ, reach, reach)
    if(hit <= 1){
      suc = 1
      break
    }
  }
  output <- list(final_x = x_pos, final_y = y_pos, success = suc)
  return(output)
}
