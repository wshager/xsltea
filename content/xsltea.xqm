(: XSLTea: An XSLT implementation in Xquery :)
(: Author: Wouter Hager :)
(: Copyright: Lagua 2016 :)

xquery version "3.1";

module namespace xsltea="http://lagua.nl/xquery/xsltea";
declare namespace util="http://exist-db.org/xquery/util";
declare namespace xsl="http://www.w3.org/1999/XSL/Transform";
declare namespace a="http://lagua.nl/xquery/array-util";
declare namespace s="http://lagua.nl/xquery/seq-util";

declare function a:fold-left($array as array(item()?),$zero,$function){
    if(array:size($array) eq 0) then
        $zero
    else
        a:fold-left(array:tail($array), $function($zero, array:head($array)), $function )
};


declare function s:fold-left($seq as item()*,$zero,$function){
    if(count($seq) eq 0) then
        $zero
    else
        s:fold-left(tail($seq), $function($zero, head($seq)), $function )
};

declare function s:fold-left-at($seq as item()*,$zero,$function){
    s:fold-left-at($seq,$zero,$function,1)
};

declare function s:fold-left-at($seq as item()*,$zero,$function,$at){
    if(count($seq) eq 0) then
        $zero
    else
        s:fold-left-at(tail($seq), $function($zero, head($seq), $at), $function, $at + 1)
};

declare function s:for-each-at($seq as item()*,$function) {
   for $x at $i in $seq return $function($x,$i)
};

declare function xsltea:template($c,$q,$fn) {
    xsltea:template($c,$q,$fn,1)
};
declare function xsltea:template($c,$q,$fn,$prio){
    xsltea:template($c,$q,$fn,$prio,())
};

declare function xsltea:template($c,$q,$fn,$prio,$mode){
    map:put($c,"templates",xsltea:insert-template($c("templates"),$q,$fn,$prio,$mode,(),false()))
};

declare function xsltea:template($c,$q,$fn,$prio,$mode,$import){
    map:put($c,"templates",xsltea:insert-template($c("templates"),$q,$fn,$prio,$mode,$import,false()))
};

declare function xsltea:insert-template($templates,$q as xs:string,$fn,$prio as xs:integer,$mode as xs:string?,$imported as xs:string?,$default as xs:boolean){
    (: check if there's a union :)
    if(matches($q,"\|")) then
        s:fold-left(tokenize($q,"\|"),$templates,function($pre,$cur){
            xsltea:insert-template($pre,replace($cur,"^\s+|\s+$",""),$fn,$prio,$mode,$imported,$default)
        })
    else
    (: do we have a rule for this query ? :)
    if(map:contains($templates,$q)) then
        let $rules := $templates($q)
        (: get the lowest prio rule, which should be the first :)
        let $r := array:head($rules)
(:        let $prepend := $r("imported") eq false() and $imported:)
        let $prepend := false()
        (: do we have this prio and is it not built-in? :)
        return
            if($prepend eq false() and $r("prio") eq $prio and $r("default") eq false()) then
                (: we already have this prio, so increment it :)
                xsltea:insert-template($templates,$q,$fn,$prio + 1,$mode,$imported,$default)
            else
                let $newr := map {
                    "q" := $q,
                    "prio" := $prio,
                    "mode" := $mode,
                    "weight" := xsltea:weigh-rule($q,$prio),
                    "fn" := $fn,
                    "imported":= $imported,
                    "default" := $default
                }
                return map:put($templates,$q,
                    if($prepend) then
                        array:insert-before(array:for-each($rules,function($_){
                            map:put($_,"weight",$_("weight") + 1)
                        }), 1, $newr)
                    else
                        array:append($rules,$newr)
                )
    else
        map:put($templates,$q,[
            map {
                "q" := $q,
                "prio" := $prio,
                "mode" := $mode,
                "weight" := xsltea:weigh-rule($q,$prio),
                "fn" := $fn,
                "imported":= $imported,
                "default" := $default
            }
        ])
};

declare function xsltea:weigh-rule($q,$prio) {
    let $parts := tokenize($q,"\[")
    let $prio := if(count($parts) > 1) then $prio + .5 else $prio
    let $q := $parts[1]
    let $prio := if(matches($q,"(^|[^:])\*[^:]")) then $prio - .5 else $prio
    let $prio := if(matches($q,":\*")) then $prio - .25 else $prio
    let $prio := if(matches($q,"^(node|element|attribute|text|comment|processing-instruction)\(\)")) then $prio - .5 else $prio
    let $prio := if(matches($q,"/")) then $prio + .5 else $prio
    return $prio
};

declare function xsltea:value-of($c,$node){
    $node/data()
};

declare function xsltea:create-context($root){
    xsltea:create-context($root,map {})
};

declare function xsltea:create-context($root,$params){
    xsltea:create-context($root,$params,map {})
};

declare function xsltea:create-context($root,$params,$nss){
    let $templates := map {}
    let $nss := map:for-each-entry($nss, function($key,$val){
        util:declare-namespace($key, $val)
    })
    let $templates := xsltea:insert-template($templates,"/|*",function($c,$n){
        xsltea:apply-templates($c,$n/node(),(),false())
    },1,(),(),true())
    let $templates := xsltea:insert-template($templates,"text()|@*",function($c,$n){
        $n/data()
    },1,(),(),true())
    return
        map {
            "xmlns" := map {},
            "keys" := map {},
            "result":= (),
            "root" := $root,
            "frame": $params,
            "import" := (),
            "templates" := $templates
        }
};

declare function xsltea:get-ancestor-match($anc,$node){
    let $match := name($node) eq head($anc)
    return
        if($match and count($anc)>1) then
            xsltea:get-ancestor-match(tail($anc),$node/..)
        else
            $match
};

declare function xsltea:get-match($q,$node){
    if(matches($q,"^\*")) then
        $node instance of element()
    else if(matches($q,"^@\*")) then
        $node instance of attribute()
    else if(matches($q,"/")) then
        let $parts := reverse(tokenize($q,"/"))
        return
            if(xsltea:get-ancestor-match(tail($parts),$node/..)) then
                xsltea:get-match(head($parts),$node)
            else
                false()
    else
        util:eval("$node/self::" || $q)
    (:
    if(matches($q,"^\*")) then
        $node instance of element()
    else if(matches($q,"^@\*")) then
        $node instance of attribute()
    else if(matches($q,"^(node|element|attribute|text|comment|processing-instruction)\(")) then
        util:eval("$node instance of " || $q)
    else if(matches($q,"/")) then
        let $parts := reverse(tokenize($q,"/"))
        return
            if(xsltea:get-ancestor-match(tail($parts),$node/..)) then
                xsltea:get-match(head($parts),$node)
            else
                false()
    else if(matches($q,"\[")) then
        let $parts := tokenize($q,"\[")
        return
            if(xsltea:get-match(head($parts),$node)) then
                let $ret := util:eval("$node[" || string-join(tail($parts),"["))
                return exists($ret)
            else
                false()
    else if($node instance of element() or $node instance of attribute()) then
        name($node) eq $q
    else
        $node eq $q
        :)
};

declare function xsltea:apply-templates($context){
    let $node :=
        if(map:contains($context,"current")) then
            $context("current")/node()
        else
            $context("root")
    return xsltea:apply-templates($context,$node)
};

declare function xsltea:apply-templates($context,$node){
    xsltea:apply-templates($context,$node,())
};

declare function xsltea:apply-templates($context,$node,$mode){
    xsltea:apply-templates($context,$node,$mode,false())
};

declare function xsltea:index-in-siblings($nodes,$node as node()) as xs:integer* {
    for $seq in (1 to count($nodes)) return $seq[$nodes[$seq] is $node]
};

declare function xsltea:apply-templates($context,$nodes,$mode,$converted) {
    let $ret :=
        s:fold-left-at($nodes,map:put($context,"result",()),function($pre,$node,$i){
            let $pre := map:put($pre,"current",$node)
            let $rules := array:flatten(xsltea:collect-rules($pre("templates"),$node,$mode))
            return if(count($rules) > 0) then
                (: weigh the rules (using ancient sort) :)
                let $ordered :=  for $_ in $rules
                    order by $_("default"), $_("weight") descending
                    return $_
                let $fn :=
                    if(exists($ordered)) then
                        $ordered[1]("fn")
                    else
                        error(QName("http://lagua.nl/xquery/xsltea","xsltea:error"),
                            concat("Template not present in context", if($mode) then "(with mode &quot;" || $mode || "&quot;)" else "","."))
                let $pre := map:put($pre,"match",$ordered[1]("q"))
                let $ret := $fn($pre,$node)
                return
                    if($ret instance of map(xs:string,item()?)) then
                        $ret
                    else if($ret) then
                        map:put($pre,"result",($pre("result"),$ret))
                    else
                        $pre
            else
                $pre
        })
    return
        if($converted) then
            $ret
        else
            if($ret instance of map(xs:string,item()?)) then
                $ret("result")
            else
                $ret
};

declare function xsltea:collect-rules($templates,$node,$mode){
    s:fold-left(map:keys($templates),[],function($pre,$q){
        let $match :=
            if(matches($q,"^/")) then
                (: $node should match root :)
                if($node instance of element() and empty($node/..)) then
                    if(matches($q,"^/$")) then
                        true()
                    else
                        xsltea:get-match(replace($q,"^/",""),$node)
                else
                    false()
            else
                xsltea:get-match($q,$node)
        return
            if($match) then
                (: insert the rules :)
                a:fold-left($templates($q),$pre,function($pre,$_){
                    if(empty(($mode,$_("mode"))) or $mode eq $_("mode")) then
                        array:append($pre,$_)
                    else
                        $pre
                })
            else
                $pre
    })
};

declare function xsltea:call-template($context,$name,$node){
    let $rules := $context("templates")("#" || $name)
    let $fn :=
        if(exists($rules)) then
            try {
                $rules(1)("fn")
            } catch * {
                error(QName("http://lagua.nl/xquery/xsltea","xsltea:error"),"function not found in rule " || $name)
            }
        else
            error(QName("http://lagua.nl/xquery/xsltea","xsltea:error"),"Template &quot;" || $name || "&quot; not present in context.")
    return $fn($context,$node)
};

declare function xsltea:transform($root,$xsl) {
    xsltea:transform($root,$xsl,map {})
};

declare function xsltea:transform($root,$xsl,$params) {
    xsltea:transform($root,$xsl,$params,map {})
};

declare function xsltea:transform($root,$xsl,$params,$nss) {
    xsltea:apply-templates(xsltea:convert(xsltea:create-context($root,$params,$nss),$xsl,$root))
};

declare function xsltea:resolve-doc($base,$uri) {
    let $uri := replace($uri,"^xmldb://","")
    let $uri :=
        if(matches($uri,"^/")) then
            $uri
        else
            concat(replace($base,"[^/]*$",""),"/",$uri)
    return doc($uri)/xsl:stylesheet
};

declare function xsltea:assert-names($xsl){
    for-each($xsl/node()[namespace-uri() eq xs:anyURI("http://www.w3.org/1999/XSL/Transform")],function($node){
        if(local-name($node) = (
            "param",
            "variable",
            "import",
            "template",
            "apply-templates",
            "call-template",
            "value-of",
            "copy-of",
            "copy",
            "if",
            "choose",
            "when",
            "otherwise",
            "with-param",
            "text",
            "attribute",
            "element",
            "for-each",
            "number",
            "key"
        )) then
            xsltea:assert-names($node)
        else
            error(QName("http://lagua.nl/xquery/xsltea","xsltea:error"),name($node) || " not implemented")
    })
};

declare function xsltea:import($pre,$cur,$node){
    xsltea:convert(map:put($pre,"import",$cur/@href/string()),xsltea:resolve-doc($cur/base-uri(),$cur/@href),$node,$cur/@href/string())
};

declare function xsltea:setkey($pre,$cur,$node){
    let $key :=
        map {
            "name":$cur/@name/string(),
            "match":$cur/@match/string(),
            "use":$cur/@use/string()
        }
    let $keys := map:put($pre("keys"),$cur/@name/string(),$key)
    return map:put($pre,"keys",$keys)
};

declare function xsltea:append-child($p as element(),$c as item()*) {
    if($c instance of attribute()) then
        element { name($p) } {
            $p/@*,
            $c,
            if($p/node()) then $p/node() else ()
        }
    else
        element { name($p) } {
            $p/@*,
            if($p/node()) then $p/node() else (),
            $c
        }
};

declare function xsltea:insert-attr-data($p as attribute(),$c as xs:anyAtomicType*){
    attribute { name ($p) } {
        if($p/data()) then $p/data() else (),
        $c
    }
};

declare function xsltea:insert-text-data($p as text(),$c as xs:anyAtomicType*){
    text {
        if($p/data()) then $p/data() else (),
        $c
    }
};

declare function xsltea:insert-result($c,$n){
    let $p := $c("result")
    let $ret :=
        try {
            typeswitch($p)
                case element() return xsltea:append-child($p,$n)
                case attribute() return xsltea:insert-attr-data($p,$n)
                case text() return xsltea:insert-text-data($p,$n)
                default return ($p,$n)
        } catch * {
            error(QName("http://lagua.nl/xquery/xsltea","xsltea:error"),"error in insert")
        }
    return map:put($c,"result",$ret)
};

declare function xsltea:eval($c,$node,$select){
    let $frame := $c("frame")
    let $select := replace($select,"xmldb://","")
    let $parts := tokenize(replace($select,"\$([\p{L}\p{N}\-_]+)","\$frame('$1')"),"\|")
    return
        try {
            util:eval(string-join($parts ! concat("$node/(",.,")"), "|"))
        } catch * {
            error(QName("http://lagua.nl/xquery/xsltea","xsltea:error"),"Failed to evaluated XPATH expression: &quot;" || $parts || "&quot;. " || $err:description)
        }
};

declare function xsltea:analyze-data($data) {
    let $parts := analyze-string($data,"(\}?)([^\{\}]*)(\{?)")/fn:match
    return
        if(count($parts) < 3) then
            concat("'",$data,"'")
        else
            xsltea:analyze-data($parts,(),false())
};

declare function xsltea:analyze-data($seq as item()*,$pre,$cat){
    if(empty($seq)) then
        $pre
    else
        let $cur := head($seq)
        let $str := $cur/fn:group[@nr=2]/string()
        let $opener := matches($cur/fn:group[@nr=3],"\{")
        let $closer := matches($cur/fn:group[@nr=1],"\}")
        return
            if(empty($cur/fn:group[@nr=2])) then
                xsltea:analyze-data(tail($seq),$pre,$cat)
            else if(($opener and $closer)) then
                (concat("'",$str,"'"),xsltea:analyze-data(tail($seq),$pre,$cat))
            else
                let $quoted := matches($str,"'") and count(tokenize($str,"'")) mod 2 eq 0
                let $str :=
                    if($quoted) then
                        concat($cur/fn:group[@nr=1]/string(),$str,$cur/fn:group[@nr=3]/string())
                    else if($opener or $closer) then
                        concat("'",$str,"'")
                    else
                        $str
                let $ret := xsltea:analyze-data(tail($seq),$pre,if($quoted) then not($cat) else $cat)
                return
                    if(($cat and not($quoted)) or ($quoted and not($cat))) then
                        (concat($str,head($ret)),tail($ret))
                    else
                        ($str,$ret)
};

declare function xsltea:process-element($pre,$cur,$node){
    let $frame := $pre("frame")
    let $ns := $cur/namespace-uri()
    let $elem := element { name($cur) } {
        for-each($cur/@*,function($attr) {
            let $data := $attr/data()
            return attribute { name($attr) } {
                string-join(for-each(xsltea:analyze-data($data),function($_){
                    xsltea:eval($pre,$node,$_)
                }))
            }
        })
    }
    return try {
        xsltea:insert-result($pre, xsltea:convert(map:put($pre,"result",$elem),$cur,$node)("result"))
    } catch * {
        error(QName("http://lagua.nl/xquery/xsltea","xsltea:error"),"error in node: " || $err:description)
    }
};

declare function xsltea:number($pre,$cur,$node){
    let $level :=
        if($cur/@level) then
            $cur/@level/string()
        else
            "single"
    let $from :=
        if($cur/@from) then
            xsltea:eval($pre,$node,"ancestor::" || $cur/@from/string())
        else
            xsltea:eval($pre,$node,"ancestor::" || $pre("match"))
    let $count :=
        if($cur/@count) then
            $cur/@count/string()
        else
            $pre("match")
    let $cnode := xsltea:eval($pre,$node,"ancestor-or-self::" || $count)
    let $no :=
        if($level eq "any") then
            count(xsltea:eval($pre,$cnode,"preceding-sibling::" || $count || "|ancestor-or-self::" || $count))
        else if($level eq "multiple") then
            for-each($cnode,function($_){
                count(xsltea:eval($pre,$_,"preceding-sibling::" || $count)) + 1
            })
        else
            count(xsltea:eval($pre,$cnode,"preceding-sibling::" || $count)) + 1
    return xsltea:insert-result($pre,string-join($no,"."))
};

declare function xsltea:for-each($pre,$cur,$node){
    let $seq := xsltea:eval($pre,$node,$cur/@select/string())
    let $ret :=
        for-each($seq,function($node){
            xsltea:convert(map:put($pre,"result",()),$cur,$node)("result")
        })
    return xsltea:insert-result($pre,$ret)
};

declare function xsltea:variable($pre,$cur,$node){
    let $frame := $pre("frame")
    let $name := $cur/@name/string()
    let $val := if($cur/@select) then xsltea:eval($pre,$node,$cur/@select/string()) else
        try {
             xsltea:convert(map:put($pre,"result",()),$cur,$node)("result")
        } catch * {
            error(QName("http://lagua.nl/xquery/xsltea","xsltea:error"),"error in xsltea:variable")
        }
    let $frame :=
        if(local-name($cur) eq "param" and map:contains($frame,$name)) then
            let $extern := $frame($name)
            return map:put($frame,$name,
                if(empty($extern)) then
                    $val
                else
                    $extern
            )
        else
            map:put($frame,$name,$val)
    return
        map:put($pre,"frame",$frame)
};

declare function xsltea:if($pre,$cur,$node){
    if(xsltea:eval($pre,$node,$cur/@test/string())) then
        xsltea:convert($pre,$cur,$node)
    else
        $pre
};

declare function xsltea:choose($pre,$cur,$node){
    let $ret := for-each($cur/node()[local-name(.) eq "when"],function($when) {
        xsltea:convert(map:put($pre,"result",()),element xsl:choose { $when },$node)("result")
    })
    return
        if(exists($ret)) then
            xsltea:insert-result($pre,$ret[1])
        else
            xsltea:convert($pre,$cur/node()[local-name(.) eq "otherwise"][1],$node)
};

declare function xsltea:convert($c,$xsl,$node){
    xsltea:convert($c,$xsl,$node,())
};

declare function xsltea:convert($c,$xsl,$node,$import) {
    (: if the node is an xsl:template element, fold it into the context :)
    (: if the node is some other xsl element, call the specified function :)
    (: otherwise return the node :)
    if($xsl/node()) then
        s:fold-left($xsl/node(),$c,function($pre,$cur){
            typeswitch($cur)
            case element() return
                if($cur/namespace-uri() eq "http://www.w3.org/1999/XSL/Transform") then
                    switch(local-name($cur))
                        case "import" return
                            xsltea:import($pre,$cur,$node)
                        case "param" case "variable" case "with-param" return
                            xsltea:variable($pre,$cur,$node)
                        case "key" return
                            xsltea:setkey($pre,$cur,$node)
                        case "template" return
                            let $named := exists($cur/@name)
                            let $mode := $cur/@mode/string()
                            let $tpl :=
                                if($named) then
                                    "#" || $cur/@name/string()
                                else
                                    $cur/@match/string()
                            return
                                xsltea:template(
                                    $pre,
                                    $tpl,
                                    function($c,$n) {
                                        try {
                                            xsltea:convert($c,$cur,$n)
                                        } catch * {
                                            error(QName("http://lagua.nl/xquery/xsltea","xsltea:error"),"error in application of template &quot;" || $tpl || "&quot;")
                                        }
                                    },
                                    if(exists($cur/@priority)) then
                                        number($cur/@priority)
                                    else
                                        1,
                                    $mode,
                                    $import
                                )
                        case "apply-templates" return
                            xsltea:apply-templates($pre,if($cur/@select) then xsltea:eval($pre,$node,$cur/@select/string()) else $node/node(),$cur/@mode/string(),true())
                        case "call-template" return
                            (: first insert params, but create a copy of context :)
                            xsltea:call-template(xsltea:convert($pre,$cur,$node),$cur/@name/string(),$node)
                        case "value-of" return
                            try {
                                xsltea:insert-result($pre, string-join(xsltea:eval($pre,$node,$cur/@select/string())))
                            } catch * {
                                error(QName("http://lagua.nl/xquery/xsltea","xsltea:error"),"error in value-of &quot;" || $cur/@select || "&quot;")
                            }
                        case "copy-of" return
                            xsltea:insert-result($pre,xsltea:eval($pre,$node,$cur/@select/string()))
                        case "copy" return
                            xsltea:insert-result($pre,$node)
                        case "if" case "when" return
                            xsltea:if($pre,$cur,$node)
                        case "choose" return
                            xsltea:choose($pre,$cur,$node)
                        case "text" return xsltea:insert-result($pre,text { $cur/node()})
                        case "element" return
                            xsltea:insert-result($pre,xsltea:convert(map:put($pre,"result",element { $cur/@name/string() } { }),$cur,$node)("result"))
                        case "attribute" return
                            try {
                                xsltea:insert-result($pre,xsltea:convert(map:put($pre,"result",attribute { $cur/@name/string() } { }),$cur,$node)("result"))
                            } catch * {
                                error(QName("http://lagua.nl/xquery/xsltea","xsltea:error"),"error in attr &quot;" || $cur/@name || "&quot;")
                            }
                        case "for-each" return
                            xsltea:for-each($pre,$cur,$node)
                        case "number" return
                            xsltea:number($pre,$cur,$node)
                        default return $pre
                else
                    xsltea:process-element($pre,$cur,$node)
            case text() return
                if(matches($cur,"^[&#x20;&#x9;&#xD;&#xA;]+$")) then
                    $pre
                else
                    xsltea:insert-result($pre,$cur)
            default return $pre
        })
    else
        $c
};

declare function xsltea:transpile($xsl){
    if($xsl/node()) then
        s:fold-left($xsl/node(),(),function($pre,$cur){
            typeswitch($cur)
            case element() return
                if($cur/namespace-uri() eq "http://www.w3.org/1999/XSL/Transform") then
                    switch(local-name($cur))
                        case "import" return (: this should be modular, so expect existing xql :)
                            ($pre,concat("xsltea:resolve-doc('",$cur/base-uri(),"','",replace($cur/@href,".xsl$",".xql"),"')"))
                        case "param" case "variable" case "with-param" return
                            ($pre,concat("map:put($c,'",$cur/@name,"',",if($cur/@select) then concat("$n/",$cur/@select) else xsltea:transpile($cur),")"))
                        (:case "key" return
                            xsltea:setkey($pre,$cur,$node):)
                        case "template" return
                            let $named := exists($cur/@name)
                            let $mode := $cur/@mode/string()
                            let $tpl :=
                                if($named) then
                                    "#" || $cur/@name/string()
                                else
                                    $cur/@match/string()
                            return
                                ($pre,concat("xsltea:template($c,'",$tpl,"',function($c,$n){",xsltea:transpile($cur),"},",
                                    if(exists($cur/@priority)) then
                                        number($cur/@priority)
                                    else
                                        1,
                                    $mode,")"))
                        case "apply-templates" return
                            ($pre,concat("xsltea:apply-templates($c,",if($cur/@select) then concat("$n/",$cur/@select/string()) else "$n",",'",$cur/@mode/string(),"')"))
                        case "call-template" return
                            (: first insert params, but create a copy of context :)
                            ($pre,concat("xsltea:call-template($c,'",$cur/@name/string(),"',$n)"))
                        case "value-of" return
                            ($pre,concat("string($n/",$cur/@select/string(),")"))
                        case "copy-of" return
                            ($pre,concat("$n/",$cur/@select/string()))
                        case "copy" return
                            ($pre,"$n")
                        case "if" case "when" return
                            ($pre,concat("if($n/",$cur/@test/string(),") then ",xsltea:transpile($cur),if(local-name($cur) eq "if") then " else ()" else ""))
                        case "choose" return
                            let $when := string-join($cur/*[local-name() = "when"] ! xsltea:transpile(.)," else ")
                            let $other := $cur/*[local-name() = "otherwise"]
                            return ($pre,concat($when," else ",if($other) then xsltea:transpile($other) else "()"))
                        case "text" return ($pre,concat("text { ",$cur/node(),"}"))
                        case "element" return
                            ($pre,concat("element { '",$cur/@name/string(),"' } { ",xsltea:transpile($cur)," }"))
                        case "attribute" return
                            ($pre,concat("attribute { '",$cur/@name/string(),"' } { ",xsltea:transpile($cur)," }"))
                        case "for-each" return
                            ($pre,concat("for-each($n/",$cur/@select/string(),",function($node){ ",xsltea:transpile($cur)," })"))
                        (: case "number" return
                            xsltea:number($pre,$cur,$node):)
                        default return $pre
                else
                    ()
            case text() return
                ()
            default return ()
        })
    else
        ()
};

declare function xsltea:collect-all-rules($templates){
    s:fold-left(map:keys($templates),[],function($pre,$q){
        a:fold-left($templates($q),$pre,function($pre,$_){
            array:append($pre,$_)
        })
    })
};

declare function xsltea:test($c){
    let $rules := xsltea:collect-all-rules($c("templates"))
    let $root := $c("root")
    return array:flatten(array:for-each($rules,function($rule){
        let $node :=
            if(starts-with($rule("q"),"#")) then
                ()
            else
                xsltea:eval($c,$root,$rule("q"))
        return $rule("fn")($c,$node)
    }))
};
