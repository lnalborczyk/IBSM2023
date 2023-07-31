library(DiagrammeR)

grViz("

        digraph nicegraph {
        
        graph [color = crimson]
        
        node [fontname = Helvetica, fontcolor = black,
        shape = plaintext, fixedsize = true, width = 1]
        
        femmes [label = '1000 \n femmes']
        cancer [label = '8 \n cancer']
        nocancer [label = '992 \n no cancer']
        mampos [label = '7 \n Mam=+']
        mamneg [label = '1 \n Mam=-']
        mampos2 [label = '69 \n Mam=+']
        mamneg2 [label = '923 \n Mam=-']
        
        femmes -> {cancer nocancer} [arrowhead = none]
        cancer -> {mampos mamneg} [arrowhead = none]
        nocancer -> {mampos2 mamneg2} [arrowhead = none]
        
        }
        ", width = 600, height = 600)
