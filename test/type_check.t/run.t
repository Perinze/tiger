testcases
  $ for t in $(ls testcases); do echo $t; dune exec --no-build tiger testcases/$t; done

merge.tig
$ dune exec --no-build tiger < merge.tig

queens.tig
$ dune exec --no-build tiger < queens.tig