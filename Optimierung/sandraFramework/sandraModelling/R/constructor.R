#Constructor
#'
#'create a new obejct
#'
#'@export
#'@return New object of sandra
New <- function(class_name) {
  if(class_name == "Assel"){
    return(new.Assel())
  }
  if(class_name == "PV"){
    return(new.PV())
  }
  if(class_name == "Assth"){
    return(new.Assth())
  }
  if(class_name == "Assfuel"){
    return(new.Assfuel())
  }
  if(class_name == "TS"){
    return(new.TS())
  }
  if(class_name == "FS"){
    return(new.FS())
  }
  if(class_name == "Bat"){
    return(new.Bat())
  }
  if(class_name == "CHP"){
    return(new.CHP())
  }
  if(class_name == "HePu"){
    return(new.HePu())
  }
  if(class_name == "HeSw"){
    return(new.HeSw())
  }
  if(class_name == "Root"){
    return(new.Root())
  }
  if(class_name == "Assel"){
    return(new.Assel())
  }
  if(class_name == "Wado"){
    return(new.Wado())
  }
  if(class_name == "Sandbox"){
    return(new.Sandbox())
  }
  if(class_name == "Demth"){
    return(new.Demth())
  }
  if(class_name == "Demel"){
    return(new.Demel())
  }
  if(class_name == "Demfuel"){
    return(new.Demfuel())
  }
  if(class_name == "GaBo"){
    return(new.GaBo())
  }
  if(class_name == "PubGel"){
    return(new.PubGel())
  }
  if(class_name == "PubGfuel"){
    return(new.PubGfuel())
  }
  if(class_name == "PubGth"){
    return(new.PubGth())
  }
  if(class_name == "LM"){
    return(new.LM())
  }
  if(class_name == "Prodfuel"){
    return(new.Prodfuel())
  }
  
}

