---
title: "Internal Link Demonstration"
output:
  html_document:
    fig_height: 14
    fig_width: 10
    self_contained: true
    mode: selfcontained
    theme: lumen
    toc: true
    number_sections: true
    toc_depth: 3
    toc_float:
      collapsed: true
      smooth_scroll: true
    df_print: "paged"
    keep_md: true
  pdf_document:
    toc: yes
    toc_depth: '3'
  distill::distill_article:
    toc: true
    toc_depth: 4
  editor_options: 
    chunk_output_type: console
  
---

<!--               
output:
  html_document:
    fig_height: 14
    fig_width: 10
    self_contained: true
    mode: selfcontained
    theme: cerulean
    toc: true
    number_sections: true
    toc_depth: 3
    toc_float:
      collapsed: true
      smooth_scroll: true
    df_print: "paged"
    keep_md: true
  pdf_document:
    toc: yes
    toc_depth: '3'
  distill::distill_article:
    toc: true
    toc_depth: 4
  editor_options: 
    chunk_output_type: console
  

-->


* Go to [tablex](#table)
* Go to [plot](#custom-idx)



### Plotx {#custom-idx}


``` r
mtcars %>% 
  ggplot(aes(x = hp, y = mpg)) +
  geom_point()
```

![](InternalLinkDemo_files/figure-html/plot-caption-1.png)<!-- -->

### Table


``` r
mtcars %>% 
  glimpse()
```

```
## Rows: 32
## Columns: 11
## $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8,…
## $ cyl  <dbl> 6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 8,…
## $ disp <dbl> 160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146.7, 140.8, 16…
## $ hp   <dbl> 110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, 180, 180, 180…
## $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.92, 3.92,…
## $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, 3.150, 3.…
## $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20.00, 22.90, 18…
## $ vs   <dbl> 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0,…
## $ am   <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0,…
## $ gear <dbl> 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3, 3,…
## $ carb <dbl> 4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2, 1, 1, 2,…
```
