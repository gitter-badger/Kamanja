# Json transformation models (jtm)

## JTM format

### Conventions

List: array of values 
    name: ["value1", "value2", ..., "valueN"]

Map: and object with key value pairs. Value can be string or objects 

```javascript
    name : {
        "key1": "value1",
        "key2": "value2",
        "keyN": "valueN"        
    }

    name : {
        "key1": {...},
        "key2": {...},
        "keyN": {...}        
    }
```

References

    If an expression needs to reference an external element it need to be marked with a $element

    $element or ${element} - references something in the current name space like a alias or a generated variable. There referenced element must be unique
    ${alias.element} - references something in the current alias name space

    Mapper messages always need to be in the form of ${alias.element}

### The 5 main sections

Header - specifies the "target" language, version and the namespace. Those three elements are used during the code generation. There is also a version attribute, add missing Requirements

```javascript
  "header": {
    "namespace": "com.ligadata.jtm.test.filter",
    "version": "0.0.1",
    "language": "scala",
    "minVersion": "2.11",
    "description": "Add a short description here"
    "name": "className"
  }
``
Imports - specify  additional packages/libraries to be used

```javascript
  "imports": {
    "packages": [ "com.ligadata.Utils._", "com.ligadata.runtime._" ]
  }
```

Grok - specifies the grok instances that can be used later in the transformations. Current limit is 1.

```javascript
  "grok": {
      "name_grok_instance" : {
        "builtInDictionary": true,
        "match": "{EMAIL: email}",
        "file": ["path"],
        "patterns_dis": {
          "DOMAINTLD": "[a-zA-Z]+",
          "EMAIL": "%{NOTSPACE}@%{WORD}\\.%{DOMAINTLD}"
        }
      }
  }
```

Aliases - specify aliases for long type names to be used in the transformation section, reference to what messages, concepts and variables are.

```javascript
  "aliases": {
    "messages": {
      "mm1": "com.ligadata.kamanja.test.msg5",
      "mo1": "com.ligadata.kamanja.test.msg2"
    },
    "concepts": {
       "cm1": "com.ligadata.kamanja.test.msg5",
       "co1": "com.ligadata.kamanja.test.msg2"
    },
    "variables": {
       "cm1": "com.ligadata.kamanja.test.msg5",
       "co1": "com.ligadata.kamanja.test.msg2"
    }
  }
```

Transformations

    Transformations section sepcifies induvidual named transformations. Each transformation can contain multiple outputs.

    DependsOn specifies set of messages that are require before a transformation can fire
    
    Computes: list with computations
    
    Outputs: specifies the ouputs the transformation section can produce. It can contain a mapping and computes list and a single where condition
        
        Mapping map - key output target, value can contain a variable or expression
        
        Where value if the expression evaluates to true processing will continue
        
        Computes: map with computations
    
        "grok matches": map with grok matches

```javascript
    "transformations": {
        "test1": {
          "dependsOn": [ ["m1"],  ["m2"]],
          "computes": {
            "out3":{ "type": "Int", "vals": ["$m1.in1 + 1000", "$m2.in1 + 2000"], "comment": "in scala, type could be optional" }
          },
    
          "grok match": {
            "in1": "{EMAIL: email} {DOMAIN: domain} {USER: user}"
          },
          
          "outputs": {
            "o1": {
              "mapping": {
                "out1": "in1",
                "out2": "t1",
                "out4": "in3"
              },
    
              "where": "!($in2 != -1 && $in2 < 100)",
    
              "computes": {
                "t1": { "type": "String", "val": "\"s:\" + ($in2).toString()" }
              }
            }
          }
        }
      }
```
    Comment and Comments:
    
    The forllowing sections support comments - compute object, transformation object

```javascript    
    { comment: "single line comment" }
    { comments: ["multiple", "lines", "comment"] }
```

### Deep-dive transformation section 


===================== Other =============================


jtm structure:
        1. grok match - common grok match expressions across all transformations
        2. computes - common computed values used by all transformations
        3. each named transformation contains the following
            a. dependsOn - an array of sets; where this named transformation triggers when one of the sets satisfy
               each set can contain messages or concepts or variables or combination of them.
            b. grok match - matching section that can contain one or more grok pattern match expressions
            c. computes - transformation level computed values; these could be shared across all outputs
            d. outputs - zero or more output definitions

Allows to specify transformation in a json file that is translated to scala code and is executed as a model 
<TBT>

Jtm Language

input - input message
where - filter expressions
output - output message
<TBT>

Compiler

Run the compiler manual.
<TBT> 

sbt (jtm)> run --help
[info] Running com.ligadata.jtm.Transpiler --help
[info]   -j, --jtm  <arg>   Sources to compile (default = )
[info]       --help            Show help message
[success] Total time: 3 s, completed Jan 18, 2016 10:42:03 PM
