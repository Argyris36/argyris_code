library(DiagrammeR) # haven't used this in a while, we can customise better

grViz("
digraph awareness_campaign {

  # we can change the node styles
  node [shape = box, style = filled, fillcolor = lightblue, fontsize = 12, fontname = Helvetica]

  # nodes, I took the naems from the intro
  information_intervention [label = 'Information Intervention', shape = plaintext, fillcolor = none, fontcolor = black]
  susceptible [label = 'Susceptible']
  non_susceptible [label = 'Non-Susceptible']
  aware [label = 'Aware']
  non_aware [label = 'Non-Aware']
  help_seeking [label = 'Help-Seeking']
  not_help_seeking [label = 'Not Help-Seeking']
  diagnosed [label = 'Diagnosed']
  non_diagnosed [label = 'Non-Diagnosed']
  treated [label = 'Treated']
  non_treated [label = 'Non-Treated']
  benefited_only [label = 'Benefited Only']
  benefited_and_harmed [label = 'Benefited and Harmed']
  harmed_only [label = 'Harmed Only']
  neither [label = 'Neither Benefited nor Harmed']

 
  information_intervention -> susceptible [color = black]
  information_intervention -> non_susceptible [color = grey, style = dashed]

  susceptible -> aware [label = ' ']
  susceptible -> non_aware [label = ' ']
  aware -> help_seeking [label = ' ']
  aware -> not_help_seeking [label = ' ']
  help_seeking -> diagnosed [label = ' ']
  help_seeking -> non_diagnosed [label = ' ']
  diagnosed -> treated [label = ' ']
  diagnosed -> non_treated [label = ' ']
  treated -> benefited_only [label = ' ']
  treated -> benefited_and_harmed [label = ' ']
  treated -> harmed_only [label = ' ']
  treated -> neither [label = ' ']

  # node positions, we can change these too
  {rank=same; information_intervention}
  {rank=same; susceptible; non_susceptible}
  {rank=same; aware; non_aware}
  {rank=same; help_seeking; not_help_seeking}
  {rank=same; diagnosed; non_diagnosed}
  {rank=same; treated; non_treated}
  {rank=same; benefited_only; benefited_and_harmed; harmed_only; neither}

  # for the tope
  subgraph cluster_0 {
    label = 'Awareness Campaign'
    style = dashed
    color = grey
    # susceptible -> aware [dir=forward, style=solid]
    # susceptible -> non_aware [dir=forward, style=solid]
  }
}
")
