library(rpf)
library(ggplot2)
library(reshape2)

#options(shiny.reactlog=TRUE)

verbose <- FALSE

moveParameter <- function(input, state, newValue) {
  whichPar <- isolate(input$editPar)
  pi <- rpf.paramInfo(isolate(state$spec), match(whichPar, names(isolate(state$par))))
  if (!is.na(pi$lower) && newValue < pi$lower) newValue <- pi$lower
  if (!is.na(pi$upper) && newValeu > pi$upper) newValue <- pi$upper
  if (verbose) cat("moveParameter", whichPar,"to",newValue, fill=T)
  oldValue <- isolate(state$par[whichPar])
  doit <- newValue != oldValue
  if (doit) {
    state$par[whichPar] <- newValue
  }
  return(doit)
}

shinyServer(function(input, output, session) {
  state <- reactiveValues(spec = NULL, par = NULL)
  
  observe({
     newVal <- input$editParValue
     if (is.null(isolate(state$spec))) return()
     if (!is.numeric(newVal)) return()
     moveParameter(input, state, newVal)
   })
  
  observe({
    hit <- input$setParValue0
    if (is.null(isolate(state$spec))) return()
    if (verbose) cat("set0",hit,fill=T)
    if (moveParameter(input, state, 0)) {
      updateSliderInput(session, "editParValue", value = 0)
    }
  })
  
  observe({
    hit <- input$setParValue1
    if (is.null(isolate(state$spec))) return()
    if (verbose) cat("set1",hit,fill=T)
    if (moveParameter(input, state, 1)) {
      updateSliderInput(session, "editParValue", value = 1)
    }
  })
  
  observe({
    if (verbose) cat("reset to new model/outcome", fill=T)
    Ta <- input$nominalTa
    Tc <- input$nominalTc
    spec <- switch(input$model,
                   'dichotomous' = rpf.drm(),
                   'graded' = rpf.grm(input$outcomes),
                   'nominal' = rpf.nrm(input$outcomes, T.a=Ta, T.c=Tc))
    par <- rpf.rparam(spec)
    
    parNames <- names(rpf.rparam(spec))
    updateSelectInput(session, "editPar", choices = parNames, selected = parNames[1])
    
    state[['spec']] <- spec
    state[['par']] <- par
  })

  observe({
    whichPar <- input$editPar
    val <- isolate(state$par[whichPar])
    if (verbose) cat("switch editing parameter to", whichPar, val, fill = TRUE)
    updateSliderInput(session, "editParValue",
                      label = paste("Parameter", whichPar), value = as.numeric(val))
  })

  output$parView <- renderTable({
    data.frame(par=state$par)
  })
  
  output$plot1 <- renderPlot({
    width <- 4
    grid <- expand.grid(theta=seq(-width,width,.1))
    
    trace <- try(rpf.prob(state$spec, state$par, grid$theta), silent = TRUE)
    if (inherits(trace, "try-error") || any(is.na(trace))) {
      pl <- ggplot(grid, aes(theta, 0)) + geom_line() + ylim(0,1) +
        geom_text(label="Invalid parameters", y=.5, x=0, size=14, color="red")
      return(pl)
    }
    grid <- cbind(grid, t(trace))
    grid2 <- melt(grid, id.vars=c("theta"), variable.name="category", value.name="p")
    
    ggplot(grid2, aes(theta, p, color=category)) + geom_line() +
      ylim(0,1) + xlim(-width, width)
  })
  
})

# runApp('.',display.mode="showcase")

