testcases
  $ for i in {1..49}; do echo "#$i"; dune exec --no-build tiger testcases/"test$i.tig"; done

merge.tig
  $ dune exec --no-build tiger < merge.tig

queens.tig
  $ dune exec --no-build tiger < queens.tig