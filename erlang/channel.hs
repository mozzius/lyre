ast
  = ( Constr
      ( Module
        (Atom "channel")
        [ Function (Atom "channel", 0)
        , Function (Atom "channel", 1)
        , Function (Atom "close", 1)
        , Function (Atom "module_info", 0)
        , Function (Atom "module_info", 1)
        , Function (Atom "recv", 1)
        , Function (Atom "send", 2)
        , Function (Atom "test", 0)
        , Function (Atom "test", 1)
        ]
        [ ( Atom "file"
          , CList
            ( L
              [ CTuple
                  [ CList
                    ( LL
                      [CLit (LInt 115)]
                      ( CList
                        ( LL
                          [CLit (LInt 114)]
                          ( CList
                            ( LL
                              [CLit (LInt 99)]
                              ( CList
                                ( LL
                                  [CLit (LInt 47)]
                                  ( CList
                                    ( LL
                                      [CLit (LInt 116)]
                                      ( CList
                                        ( LL
                                          [CLit (LInt 101)]
                                          ( CList
                                            ( LL
                                              [CLit (LInt 115)]
                                              ( CList
                                                ( LL
                                                  [CLit (LInt 116)]
                                                  ( CList
                                                    ( LL
                                                      [CLit (LInt 46)]
                                                      ( CList
                                                        ( LL
                                                          [CLit (LInt 101)]
                                                          ( CList
                                                            ( LL
                                                              [CLit (LInt 114)]
                                                              ( CList
                                                                ( L
                                                                  [ CLit
                                                                      (LInt 108)
                                                                  ]
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  , CLit (LInt 1)
                  ]
              ]
            )
          )
        ]
        [ FunDef
          (Constr (Function (Atom "channel", 0)))
          ( Constr
            ( Lambda
              []
              ( Exp
                ( Constr
                  ( ModCall
                    ( Exp (Constr (Lit (LAtom (Atom "erlang"))))
                    , Exp (Constr (Lit (LAtom (Atom "spawn"))))
                    )
                    [ Exp (Constr (Lit (LAtom (Atom "channel"))))
                    , Exp (Constr (Lit (LAtom (Atom "channel"))))
                    , Exp (Constr (List (L [Exp (Constr (Lit LNil))])))
                    ]
                  )
                )
              )
            )
          )
        , FunDef
          (Constr (Function (Atom "channel", 1)))
          ( Constr
            ( Lambda
              ["_0"]
              ( Exp
                ( Constr
                  ( Case
                    (Exp (Constr (Var "_0")))
                    [ Constr
                      ( Alt
                        (Pats [PLit LNil])
                        (Guard (Exp (Constr (Lit (LAtom (Atom "true"))))))
                        ( Exp
                          ( Constr
                            ( Rec
                              [ Constr
                                ( Alt
                                  (Pats [PLit (LAtom (Atom "close"))])
                                  ( Guard
                                    (Exp (Constr (Lit (LAtom (Atom "true")))))
                                  )
                                  (Exp (Constr (Lit (LAtom (Atom "closed")))))
                                )
                              , Constr
                                ( Alt
                                  ( Pats
                                    [ PTuple
                                        [PLit (LAtom (Atom "recv")), PVar "PID"]
                                    ]
                                  )
                                  ( Guard
                                    (Exp (Constr (Lit (LAtom (Atom "true")))))
                                  )
                                  ( Exp
                                    ( Constr
                                      ( App
                                        ( Exp
                                          ( Constr
                                            (Fun (Function (Atom "channel", 1)))
                                          )
                                        )
                                        [ Exp
                                            ( Constr
                                              ( List
                                                ( LL
                                                  [Exp (Constr (Var "PID"))]
                                                  (Exp (Constr (Lit LNil)))
                                                )
                                              )
                                            )
                                        ]
                                      )
                                    )
                                  )
                                )
                              ]
                              ( TimeOut
                                (Exp (Constr (Lit (LAtom (Atom "infinity")))))
                                (Exp (Constr (Lit (LAtom (Atom "true")))))
                              )
                            )
                          )
                        )
                      )
                    , Constr
                      ( Alt
                        (Pats [PList (LL [PVar "X"] (PVar "Xs"))])
                        (Guard (Exp (Constr (Lit (LAtom (Atom "true"))))))
                        ( Exp
                          ( Constr
                            ( Rec
                              [ Constr
                                ( Alt
                                  (Pats [PLit (LAtom (Atom "close"))])
                                  ( Guard
                                    (Exp (Constr (Lit (LAtom (Atom "true")))))
                                  )
                                  (Exp (Constr (Lit (LAtom (Atom "close")))))
                                )
                              , Constr
                                ( Alt
                                  ( Pats
                                    [ PTuple
                                        [PLit (LAtom (Atom "send")), PVar "Msg"]
                                    ]
                                  )
                                  ( Guard
                                    (Exp (Constr (Lit (LAtom (Atom "true")))))
                                  )
                                  ( Exp
                                    ( Constr
                                      ( Let
                                        ( ["_1"]
                                        , Exp
                                          ( Constr
                                            ( ModCall
                                              ( Exp
                                                ( Constr
                                                  (Lit (LAtom (Atom "erlang")))
                                                )
                                              , Exp
                                                ( Constr
                                                  (Lit (LAtom (Atom "self")))
                                                )
                                              )
                                              []
                                            )
                                          )
                                        )
                                        ( Exp
                                          ( Constr
                                            ( Seq
                                              ( Exp
                                                ( Constr
                                                  ( ModCall
                                                    ( Exp
                                                      ( Constr
                                                        ( Lit
                                                          ( LAtom
                                                            (Atom "erlang")
                                                          )
                                                        )
                                                      )
                                                    , Exp
                                                      ( Constr
                                                        (Lit (LAtom (Atom "!")))
                                                      )
                                                    )
                                                    [ Exp (Constr (Var "X"))
                                                    , Exp
                                                      ( Constr
                                                        ( Tuple
                                                          [ Exp
                                                            (Constr (Var "_1"))
                                                          , Exp
                                                            (Constr (Var "Msg"))
                                                          ]
                                                        )
                                                      )
                                                    ]
                                                  )
                                                )
                                              )
                                              ( Exp
                                                ( Constr
                                                  ( App
                                                    ( Exp
                                                      ( Constr
                                                        ( Fun
                                                          ( Function
                                                            (Atom "channel", 1)
                                                          )
                                                        )
                                                      )
                                                    )
                                                    [Exp (Constr (Var "Xs"))]
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              , Constr
                                ( Alt
                                  ( Pats
                                    [ PTuple
                                        [PLit (LAtom (Atom "recv")), PVar "PID"]
                                    ]
                                  )
                                  ( Guard
                                    (Exp (Constr (Lit (LAtom (Atom "true")))))
                                  )
                                  ( Exp
                                    ( Constr
                                      ( Let
                                        ( ["_2"]
                                        , Exp
                                          ( Constr
                                            ( List
                                              ( LL
                                                [Exp (Constr (Var "X"))]
                                                ( Exp
                                                  ( Constr
                                                    ( ModCall
                                                      ( Exp
                                                        ( Constr
                                                          ( Lit
                                                            ( LAtom
                                                              (Atom "erlang")
                                                            )
                                                          )
                                                        )
                                                      , Exp
                                                        ( Constr
                                                          ( Lit
                                                            (LAtom (Atom "++"))
                                                          )
                                                        )
                                                      )
                                                      [ Exp (Constr (Var "Xs"))
                                                      , Exp
                                                        ( Constr
                                                          ( List
                                                            ( LL
                                                              [ Exp
                                                                  ( Constr
                                                                    (Var "PID")
                                                                  )
                                                              ]
                                                              ( Exp
                                                                ( Constr
                                                                  (Lit LNil)
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      ]
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                        ( Exp
                                          ( Constr
                                            ( App
                                              ( Exp
                                                ( Constr
                                                  ( Fun
                                                    ( Function
                                                      (Atom "channel", 1)
                                                    )
                                                  )
                                                )
                                              )
                                              [Exp (Constr (Var "_2"))]
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              ]
                              ( TimeOut
                                (Exp (Constr (Lit (LAtom (Atom "infinity")))))
                                (Exp (Constr (Lit (LAtom (Atom "true")))))
                              )
                            )
                          )
                        )
                      )
                    , Ann
                      ( Alt
                        (Pats [PVar "_3"])
                        (Guard (Exp (Constr (Lit (LAtom (Atom "true"))))))
                        ( Exp
                          ( Ann
                            ( Op
                              (Atom "match_fail")
                              [ Exp
                                  ( Constr
                                    ( Tuple
                                      [ Exp
                                        ( Constr
                                          (Lit (LAtom (Atom "function_clause")))
                                        )
                                      , Exp (Constr (Var "_3"))
                                      ]
                                    )
                                  )
                              ]
                            )
                            [ CTuple
                                [ CLit (LAtom (Atom "function_name"))
                                , CTuple
                                  [CLit (LAtom (Atom "channel")), CLit (LInt 1)]
                                ]
                            ]
                          )
                        )
                      )
                      [CLit (LAtom (Atom "compiler_generated"))]
                    ]
                  )
                )
              )
            )
          )
        , FunDef
          (Constr (Function (Atom "recv", 1)))
          ( Constr
            ( Lambda
              ["_0"]
              ( Exp
                ( Constr
                  ( Let
                    ( ["_1"]
                    , Exp
                      ( Constr
                        ( ModCall
                          ( Exp (Constr (Lit (LAtom (Atom "erlang"))))
                          , Exp (Constr (Lit (LAtom (Atom "self"))))
                          )
                          []
                        )
                      )
                    )
                    ( Exp
                      ( Constr
                        ( Seq
                          ( Exp
                            ( Constr
                              ( ModCall
                                ( Exp (Constr (Lit (LAtom (Atom "erlang"))))
                                , Exp (Constr (Lit (LAtom (Atom "!"))))
                                )
                                [ Exp (Constr (Var "_0"))
                                , Exp
                                  ( Constr
                                    ( Tuple
                                      [ Exp (Constr (Lit (LAtom (Atom "recv"))))
                                      , Exp (Constr (Var "_1"))
                                      ]
                                    )
                                  )
                                ]
                              )
                            )
                          )
                          ( Exp
                            ( Constr
                              ( Rec
                                [ Constr
                                    ( Alt
                                      (Pats [PTuple [PVar "_3", PVar "Msg"]])
                                      ( Guard
                                        ( Exp
                                          ( Constr
                                            ( ModCall
                                              ( Exp
                                                ( Constr
                                                  (Lit (LAtom (Atom "erlang")))
                                                )
                                              , Exp
                                                ( Constr
                                                  (Lit (LAtom (Atom "=:=")))
                                                )
                                              )
                                              [ Exp (Constr (Var "_3"))
                                              , Exp (Constr (Var "_0"))
                                              ]
                                            )
                                          )
                                        )
                                      )
                                      (Exp (Constr (Var "Msg")))
                                    )
                                ]
                                ( TimeOut
                                  (Exp (Constr (Lit (LAtom (Atom "infinity")))))
                                  (Exp (Constr (Lit (LAtom (Atom "true")))))
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        , FunDef
          (Constr (Function (Atom "send", 2)))
          ( Constr
            ( Lambda
              ["_0", "_1"]
              ( Exp
                ( Constr
                  ( ModCall
                    ( Exp (Constr (Lit (LAtom (Atom "erlang"))))
                    , Exp (Constr (Lit (LAtom (Atom "!"))))
                    )
                    [ Exp (Constr (Var "_0"))
                    , Exp
                      ( Constr
                        ( Tuple
                          [ Exp (Constr (Lit (LAtom (Atom "send"))))
                          , Exp (Constr (Var "_1"))
                          ]
                        )
                      )
                    ]
                  )
                )
              )
            )
          )
        , FunDef
          (Constr (Function (Atom "close", 1)))
          ( Constr
            ( Lambda
              ["_0"]
              ( Exp
                ( Constr
                  ( ModCall
                    ( Exp (Constr (Lit (LAtom (Atom "erlang"))))
                    , Exp (Constr (Lit (LAtom (Atom "!"))))
                    )
                    [ Exp (Constr (Var "_0"))
                    , Exp (Constr (Lit (LAtom (Atom "close"))))
                    ]
                  )
                )
              )
            )
          )
        , FunDef
          (Constr (Function (Atom "test", 0)))
          ( Constr
            ( Lambda
              []
              ( Exp
                ( Constr
                  ( Let
                    ( ["Channel"]
                    , Exp
                      ( Constr
                        ( App
                          (Exp (Constr (Fun (Function (Atom "channel", 0)))))
                          []
                        )
                      )
                    )
                    ( Exp
                      ( Constr
                        ( Seq
                          ( Exp
                            ( Constr
                              ( App
                                (Exp (Constr (Fun (Function (Atom "send", 2)))))
                                [ Exp (Constr (Var "Channel"))
                                , Exp
                                  ( Constr
                                    ( List
                                      ( LL
                                        [Exp (Constr (Lit (LInt 104)))]
                                        ( Exp
                                          ( Constr
                                            ( List
                                              ( LL
                                                [Exp (Constr (Lit (LInt 101)))]
                                                ( Exp
                                                  ( Constr
                                                    ( List
                                                      ( LL
                                                        [ Exp
                                                            ( Constr
                                                              (Lit (LInt 108))
                                                            )
                                                        ]
                                                        ( Exp
                                                          ( Constr
                                                            ( List
                                                              ( LL
                                                                [ Exp
                                                                    ( Constr
                                                                      ( Lit
                                                                        ( LInt
                                                                          108
                                                                        )
                                                                      )
                                                                    )
                                                                ]
                                                                ( Exp
                                                                  ( Constr
                                                                    ( List
                                                                      ( L
                                                                        [ Exp
                                                                            ( Constr
                                                                              ( Lit
                                                                                ( LInt
                                                                                  111
                                                                                )
                                                                              )
                                                                            )
                                                                        ]
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                ]
                              )
                            )
                          )
                          ( Exp
                            ( Constr
                              ( Seq
                                ( Exp
                                  ( Constr
                                    ( ModCall
                                      ( Exp
                                        (Constr (Lit (LAtom (Atom "timer"))))
                                      , Exp
                                        (Constr (Lit (LAtom (Atom "sleep"))))
                                      )
                                      [Exp (Constr (Lit (LInt 500)))]
                                    )
                                  )
                                )
                                ( Exp
                                  ( Constr
                                    ( Seq
                                      ( Exp
                                        ( Constr
                                          ( ModCall
                                            ( Exp
                                              ( Constr
                                                (Lit (LAtom (Atom "erlang")))
                                              )
                                            , Exp
                                              ( Constr
                                                (Lit (LAtom (Atom "spawn")))
                                              )
                                            )
                                            [ Exp
                                              ( Constr
                                                (Lit (LAtom (Atom "channel")))
                                              )
                                            , Exp
                                              ( Constr
                                                (Lit (LAtom (Atom "test")))
                                              )
                                            , Exp
                                              ( Constr
                                                ( List
                                                  ( LL
                                                    [ Exp
                                                        (Constr (Var "Channel"))
                                                    ]
                                                    (Exp (Constr (Lit LNil)))
                                                  )
                                                )
                                              )
                                            ]
                                          )
                                        )
                                      )
                                      ( Exp
                                        ( Constr
                                          ( Seq
                                            ( Exp
                                              ( Constr
                                                ( ModCall
                                                  ( Exp
                                                    ( Constr
                                                      ( Lit
                                                        (LAtom (Atom "timer"))
                                                      )
                                                    )
                                                  , Exp
                                                    ( Constr
                                                      ( Lit
                                                        (LAtom (Atom "sleep"))
                                                      )
                                                    )
                                                  )
                                                  [ Exp
                                                      (Constr (Lit (LInt 500)))
                                                  ]
                                                )
                                              )
                                            )
                                            ( Exp
                                              ( Constr
                                                ( Seq
                                                  ( Exp
                                                    ( Constr
                                                      ( App
                                                        ( Exp
                                                          ( Constr
                                                            ( Fun
                                                              ( Function
                                                                (Atom "send", 2)
                                                              )
                                                            )
                                                          )
                                                        )
                                                        [ Exp
                                                          ( Constr
                                                            (Var "Channel")
                                                          )
                                                        , Exp
                                                          ( Constr
                                                            ( List
                                                              ( LL
                                                                [ Exp
                                                                    ( Constr
                                                                      ( Lit
                                                                        ( LInt
                                                                          119
                                                                        )
                                                                      )
                                                                    )
                                                                ]
                                                                ( Exp
                                                                  ( Constr
                                                                    ( List
                                                                      ( LL
                                                                        [ Exp
                                                                            ( Constr
                                                                              ( Lit
                                                                                ( LInt
                                                                                  111
                                                                                )
                                                                              )
                                                                            )
                                                                        ]
                                                                        ( Exp
                                                                          ( Constr
                                                                            ( List
                                                                              ( LL
                                                                                [ Exp
                                                                                    ( Constr
                                                                                      ( Lit
                                                                                        ( LInt
                                                                                          114
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                ]
                                                                                ( Exp
                                                                                  ( Constr
                                                                                    ( List
                                                                                      ( LL
                                                                                        [ Exp
                                                                                            ( Constr
                                                                                              ( Lit
                                                                                                ( LInt
                                                                                                  108
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                        ]
                                                                                        ( Exp
                                                                                          ( Constr
                                                                                            ( List
                                                                                              ( L
                                                                                                [ Exp
                                                                                                    ( Constr
                                                                                                      ( Lit
                                                                                                        ( LInt
                                                                                                          100
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                ]
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        ]
                                                      )
                                                    )
                                                  )
                                                  ( Exp
                                                    ( Constr
                                                      ( Seq
                                                        ( Exp
                                                          ( Constr
                                                            ( ModCall
                                                              ( Exp
                                                                ( Constr
                                                                  ( Lit
                                                                    ( LAtom
                                                                      ( Atom
                                                                        "timer"
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              , Exp
                                                                ( Constr
                                                                  ( Lit
                                                                    ( LAtom
                                                                      ( Atom
                                                                        "sleep"
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                              [ Exp
                                                                  ( Constr
                                                                    ( Lit
                                                                      (LInt 500)
                                                                    )
                                                                  )
                                                              ]
                                                            )
                                                          )
                                                        )
                                                        ( Exp
                                                          ( Constr
                                                            ( App
                                                              ( Exp
                                                                ( Constr
                                                                  ( Fun
                                                                    ( Function
                                                                      ( Atom
                                                                        "close"
                                                                      , 1
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                              [ Exp
                                                                  ( Constr
                                                                    ( Var
                                                                      "Channel"
                                                                    )
                                                                  )
                                                              ]
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        , FunDef
          (Constr (Function (Atom "test", 1)))
          ( Constr
            ( Lambda
              ["_0"]
              ( Exp
                ( Constr
                  ( Let
                    ( ["_1"]
                    , Exp
                      ( Constr
                        ( App (Exp (Constr (Fun (Function (Atom "recv", 1)))))
                              [Exp (Constr (Var "_0"))]
                        )
                      )
                    )
                    ( Exp
                      ( Constr
                        ( Seq
                          ( Exp
                            ( Constr
                              ( ModCall
                                ( Exp (Constr (Lit (LAtom (Atom "io"))))
                                , Exp (Constr (Lit (LAtom (Atom "format"))))
                                )
                                [ Exp
                                  ( Constr
                                    ( List
                                      ( LL
                                        [Exp (Constr (Lit (LInt 82)))]
                                        ( Exp
                                          ( Constr
                                            ( List
                                              ( LL
                                                [Exp (Constr (Lit (LInt 101)))]
                                                ( Exp
                                                  ( Constr
                                                    ( List
                                                      ( LL
                                                        [ Exp
                                                            ( Constr
                                                              (Lit (LInt 99))
                                                            )
                                                        ]
                                                        ( Exp
                                                          ( Constr
                                                            ( List
                                                              ( LL
                                                                [ Exp
                                                                    ( Constr
                                                                      ( Lit
                                                                        ( LInt
                                                                          118
                                                                        )
                                                                      )
                                                                    )
                                                                ]
                                                                ( Exp
                                                                  ( Constr
                                                                    ( List
                                                                      ( LL
                                                                        [ Exp
                                                                            ( Constr
                                                                              ( Lit
                                                                                ( LInt
                                                                                  58
                                                                                )
                                                                              )
                                                                            )
                                                                        ]
                                                                        ( Exp
                                                                          ( Constr
                                                                            ( List
                                                                              ( LL
                                                                                [ Exp
                                                                                    ( Constr
                                                                                      ( Lit
                                                                                        ( LInt
                                                                                          32
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                ]
                                                                                ( Exp
                                                                                  ( Constr
                                                                                    ( List
                                                                                      ( LL
                                                                                        [ Exp
                                                                                            ( Constr
                                                                                              ( Lit
                                                                                                ( LInt
                                                                                                  126
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                        ]
                                                                                        ( Exp
                                                                                          ( Constr
                                                                                            ( List
                                                                                              ( LL
                                                                                                [ Exp
                                                                                                    ( Constr
                                                                                                      ( Lit
                                                                                                        ( LInt
                                                                                                          112
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                ]
                                                                                                ( Exp
                                                                                                  ( Constr
                                                                                                    ( List
                                                                                                      ( LL
                                                                                                        [ Exp
                                                                                                            ( Constr
                                                                                                              ( Lit
                                                                                                                ( LInt
                                                                                                                  126
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                        ]
                                                                                                        ( Exp
                                                                                                          ( Constr
                                                                                                            ( List
                                                                                                              ( L
                                                                                                                [ Exp
                                                                                                                    ( Constr
                                                                                                                      ( Lit
                                                                                                                        ( LInt
                                                                                                                          110
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                ]
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                , Exp
                                  ( Constr
                                    ( List
                                      ( LL [Exp (Constr (Var "_1"))]
                                           (Exp (Constr (Lit LNil)))
                                      )
                                    )
                                  )
                                ]
                              )
                            )
                          )
                          ( Exp
                            ( Constr
                              ( Let
                                ( ["_2"]
                                , Exp
                                  ( Constr
                                    ( App
                                      ( Exp
                                        ( Constr
                                          (Fun (Function (Atom "recv", 1)))
                                        )
                                      )
                                      [Exp (Constr (Var "_0"))]
                                    )
                                  )
                                )
                                ( Exp
                                  ( Constr
                                    ( ModCall
                                      ( Exp (Constr (Lit (LAtom (Atom "io"))))
                                      , Exp
                                        (Constr (Lit (LAtom (Atom "format"))))
                                      )
                                      [ Exp
                                        ( Constr
                                          ( List
                                            ( LL
                                              [Exp (Constr (Lit (LInt 82)))]
                                              ( Exp
                                                ( Constr
                                                  ( List
                                                    ( LL
                                                      [ Exp
                                                          ( Constr
                                                            (Lit (LInt 101))
                                                          )
                                                      ]
                                                      ( Exp
                                                        ( Constr
                                                          ( List
                                                            ( LL
                                                              [ Exp
                                                                  ( Constr
                                                                    ( Lit
                                                                      (LInt 99)
                                                                    )
                                                                  )
                                                              ]
                                                              ( Exp
                                                                ( Constr
                                                                  ( List
                                                                    ( LL
                                                                      [ Exp
                                                                          ( Constr
                                                                            ( Lit
                                                                              ( LInt
                                                                                118
                                                                              )
                                                                            )
                                                                          )
                                                                      ]
                                                                      ( Exp
                                                                        ( Constr
                                                                          ( List
                                                                            ( LL
                                                                              [ Exp
                                                                                  ( Constr
                                                                                    ( Lit
                                                                                      ( LInt
                                                                                        58
                                                                                      )
                                                                                    )
                                                                                  )
                                                                              ]
                                                                              ( Exp
                                                                                ( Constr
                                                                                  ( List
                                                                                    ( LL
                                                                                      [ Exp
                                                                                          ( Constr
                                                                                            ( Lit
                                                                                              ( LInt
                                                                                                32
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                      ]
                                                                                      ( Exp
                                                                                        ( Constr
                                                                                          ( List
                                                                                            ( LL
                                                                                              [ Exp
                                                                                                  ( Constr
                                                                                                    ( Lit
                                                                                                      ( LInt
                                                                                                        126
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                              ]
                                                                                              ( Exp
                                                                                                ( Constr
                                                                                                  ( List
                                                                                                    ( LL
                                                                                                      [ Exp
                                                                                                          ( Constr
                                                                                                            ( Lit
                                                                                                              ( LInt
                                                                                                                112
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                      ]
                                                                                                      ( Exp
                                                                                                        ( Constr
                                                                                                          ( List
                                                                                                            ( LL
                                                                                                              [ Exp
                                                                                                                  ( Constr
                                                                                                                    ( Lit
                                                                                                                      ( LInt
                                                                                                                        126
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                              ]
                                                                                                              ( Exp
                                                                                                                ( Constr
                                                                                                                  ( List
                                                                                                                    ( L
                                                                                                                      [ Exp
                                                                                                                          ( Constr
                                                                                                                            ( Lit
                                                                                                                              ( LInt
                                                                                                                                110
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                      ]
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      , Exp
                                        ( Constr
                                          ( List
                                            ( LL [Exp (Constr (Var "_2"))]
                                                 (Exp (Constr (Lit LNil)))
                                            )
                                          )
                                        )
                                      ]
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        , FunDef
          (Constr (Function (Atom "module_info", 0)))
          ( Constr
            ( Lambda
              []
              ( Exp
                ( Constr
                  ( ModCall
                    ( Exp (Constr (Lit (LAtom (Atom "erlang"))))
                    , Exp (Constr (Lit (LAtom (Atom "get_module_info"))))
                    )
                    [Exp (Constr (Lit (LAtom (Atom "channel"))))]
                  )
                )
              )
            )
          )
        , FunDef
          (Constr (Function (Atom "module_info", 1)))
          ( Constr
            ( Lambda
              ["_0"]
              ( Exp
                ( Constr
                  ( ModCall
                    ( Exp (Constr (Lit (LAtom (Atom "erlang"))))
                    , Exp (Constr (Lit (LAtom (Atom "get_module_info"))))
                    )
                    [ Exp (Constr (Lit (LAtom (Atom "channel"))))
                    , Exp (Constr (Var "_0"))
                    ]
                  )
                )
              )
            )
          )
        ]
      )
    )