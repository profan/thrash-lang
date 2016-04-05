open Ocamlbuild_plugin;;

Ocamlbuild_plugin.rule "dypgen"
    ~prods:["%.ml";]
    ~deps:["%.dyp"]
    begin fun env _ ->
        let dyp = env "%.dyp" in
        Cmd(S[A"dypgen"; A"--no-mli"; Px dyp])
    end;
