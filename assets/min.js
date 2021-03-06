hljs.registerLanguage("r",
  function(e){
    var r="([a-zA-Z]|\\.[a-zA-Z.])[a-zA-Z0-9._]*";
    return{
      c:[e.HCM,
      {b:r,l:r,k:
        {keyword:"function if in break next repeat else for return switch while try tryCatch stop warning require lm glm smooth.spline randomForest gam tree log10 vcovHC attach detach source setMethod setGeneric setGroupGeneric sqrt waldtest pt pnorm setClass c print SELECT FROM WHERE AND",
        literal:"NULL NA TRUE FALSE T F Inf NaN NA_integer_|10 NA_real_|10 NA_character_|10 NA_complex_|10 na.omit library",
          keyname : "mean sum poly anova s",
          models: "sample which.min predict cv.glm"
        }, 
        r:0},
      {cN:"number",b:"0[xX][0-9a-fA-F]+[Li]?\\b",r:0},
      {cN:"number",b:"\\d+(?:[eE][+\\-]?\\d*)?L\\b",r:0},
      {cN:"number",b:"\\d+\\.(?!\\d)(?:i\\b)?",r:0},
      {cN:"number",b:"\\d+(?:\\.\\d*)?(?:[eE][+\\-]?\\d*)?i?\\b",r:0},
      {cN:"number",b:"\\.\\d+(?:[eE][+\\-]?\\d*)?i?\\b",r:0},
      {cN:"pipe",b:"%>%",r:0},
      {cN:"equiv",b:"~",r:0},
      {cN:"assign",b:"<-",r:0},
      {b:"`",e:"`",r:0},
      {cN:"string",c:[e.BE],v:[{b:'"',e:'"'},{b:"'",e:"'"}]},
      {cN: "keyword", b: /(^|\s*)(:::?|\.)\w+(?=\(|$)/},
      {cN: "meta",b: /(^|\s*)\w+(?=:::?|$)/,r: 0 }, ]}});

hljs.initHighlightingOnLoad();