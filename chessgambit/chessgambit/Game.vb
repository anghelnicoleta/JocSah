Public Class Game
    Public board(7, 7) As Integer
    Public can_move(7, 7) As Boolean
    Public check_for_white(7, 7) As Boolean
    Public check_for_black(7, 7) As Boolean
    Public game_flag As Boolean = True
    Public player As Integer = 1
    Public b_in_danger As Boolean = False
    Public w_in_danger As Boolean = False
    Public Sub New()
        ''matricea pentru tabla +ve sunt pentru White_pieces -ve sunt pentru Black_pieces si 0 pentru casute goale
        board = {{5, 4, 3, 2, 1, 3, 4, 5},
                 {6, 6, 6, 6, 6, 6, 6, 6},
                 {0, 0, 0, 0, 0, 0, 0, 0},
                 {0, 0, 0, 0, 0, 0, 0, 0},
                 {0, 0, 0, 0, 0, 0, 0, 0},
                 {0, 0, 0, 0, 0, 0, 0, 0},
                 {-6, -6, -6, -6, -6, -6, -6, -6},
                 {-5, -4, -3, -2, -1, -3, -4, -5}}     ''


        re_chk_black()
        re_chk_white()


    End Sub
    ''verifica castigatorul
    Public Sub chk_winner(ByVal selected, ByVal x, ByVal y)
        show()
        If GameForm.b_king.x_pos = x And GameForm.b_king.y_pos = y And selected <> "b_king" Then
            game_flag = False
        End If
        If game_flag = False Then
            MsgBox("A castigat " & GameForm.Label1.Text)
            Exit Sub
        End If
        If GameForm.w_king.x_pos = x And GameForm.w_king.y_pos = y And selected <> "w_king" Then
            game_flag = False
        End If
        If game_flag = False Then
            MsgBox("A castigat " & GameForm.Label4.Text)
            Exit Sub
        End If
    End Sub

    'pentru a arata imaginile pieselor
    Public Sub show()

        For i As Integer = 0 To 7
            For j As Integer = 0 To 7

                Select Case board(i, j)
                    Case 1
                        GameForm.pb(i, j).Image = My.Resources.wKing
                    Case 2
                        GameForm.pb(i, j).Image = My.Resources.wQueen
                    Case 3
                        GameForm.pb(i, j).Image = My.Resources.wBishop
                    Case 4
                        GameForm.pb(i, j).Image = My.Resources.wKnight
                    Case 5
                        GameForm.pb(i, j).Image = My.Resources.wRook
                    Case 6
                        GameForm.pb(i, j).Image = My.Resources.wPawn
                    Case -1
                        GameForm.pb(i, j).Image = My.Resources.bKing
                    Case -2
                        GameForm.pb(i, j).Image = My.Resources.bQueen
                    Case -3
                        GameForm.pb(i, j).Image = My.Resources.bBishop
                    Case -4
                        GameForm.pb(i, j).Image = My.Resources.bKnight
                    Case -5
                        GameForm.pb(i, j).Image = My.Resources.bRook
                    Case -6
                        GameForm.pb(i, j).Image = My.Resources.bPawn

                    Case Else
                        GameForm.pb(i, j).Image = Nothing

                End Select
                show_chk()
            Next
        Next
    End Sub
    'pentru a selecta piesa/imaginea piesei
    Public Function selection(ByVal x, ByVal y) As String
        If game_flag = True Then
            If board(x, y) <> 0 Then
                GameForm.pb(x, y).BackgroundImage = My.Resources.selection
            End If
            Select Case board(x, y)
                Case 1
                    If player = 1 Then
                        show_possible_move(GameForm.w_king.poss_mov())
                        Return "w_king"
                    End If
                Case 2
                    If player = 1 Then
                        show_possible_move(GameForm.w_queen.poss_mov())
                        Return "w_queen"
                    End If
                Case 3
                    If player = 1 Then
                        For i As Integer = 0 To 1
                            If GameForm.w_bishop(i).ret_x = x And GameForm.w_bishop(i).ret_y = y Then
                                show_possible_move(GameForm.w_bishop(i).poss_mov())
                            End If
                        Next
                        Return "w_bishop"
                    End If
                Case 4
                    If player = 1 Then
                        For i As Integer = 0 To 1
                            If GameForm.w_knight(i).ret_x = x And GameForm.w_knight(i).ret_y = y Then
                                show_possible_move(GameForm.w_knight(i).poss_mov())
                            End If
                        Next
                        Return "w_knight"
                    End If
                Case 5
                    If player = 1 Then
                        For i As Integer = 0 To 1
                            If GameForm.w_rook(i).ret_x = x And GameForm.w_rook(i).ret_y = y Then
                                show_possible_move(GameForm.w_rook(i).poss_mov())
                            End If
                        Next
                        Return "w_rook"
                    End If
                Case 6
                    If player = 1 Then
                        For i As Integer = 0 To 7
                            If GameForm.w_pawn(i).ret_x = x And GameForm.w_pawn(i).ret_y = y Then
                                show_possible_move(GameForm.w_pawn(i).poss_mov())
                            End If
                        Next
                        Return "w_pawn"
                    End If
                Case -1
                    If player = 2 Then
                        show_possible_move(GameForm.b_king.poss_mov())
                        Return "b_king"
                    End If
                Case -2
                    If player = 2 Then
                        show_possible_move(GameForm.b_queen.poss_mov())
                        Return "b_queen"
                    End If
                Case -3
                    If player = 2 Then
                        For i As Integer = 0 To 1
                            If GameForm.b_bishop(i).ret_x = x And GameForm.b_bishop(i).ret_y = y Then
                                show_possible_move(GameForm.b_bishop(i).poss_mov())
                            End If
                        Next
                        Return "b_bishop"
                    End If
                Case -4
                    If player = 2 Then
                        For i As Integer = 0 To 1
                            If GameForm.b_knight(i).ret_x = x And GameForm.b_knight(i).ret_y = y Then
                                show_possible_move(GameForm.b_knight(i).poss_mov())
                            End If
                        Next
                        Return "b_knight"
                    End If
                Case -5
                    If player = 2 Then
                        For i As Integer = 0 To 1
                            If GameForm.b_rook(i).ret_x = x And GameForm.b_rook(i).ret_y = y Then
                                show_possible_move(GameForm.b_rook(i).poss_mov())
                            End If
                        Next
                        Return "b_rook"
                    End If
                Case -6
                    If player = 2 Then
                        For i As Integer = 0 To 7
                            If GameForm.b_pawn(i).ret_x = x And GameForm.b_pawn(i).ret_y = y Then
                                show_possible_move(GameForm.b_pawn(i).poss_mov())
                            End If
                        Next
                        Return "b_pawn"
                    End If
                Case Else
                    Return Nothing
            End Select
        End If
    End Function
    'schimba imaginea cand pleaca o piesa
    Public Sub re_back()
        For i As Integer = 0 To 7
            For j As Integer = 0 To 7
                GameForm.pb(i, j).BackgroundImage = Nothing
            Next
        Next
    End Sub

    Public Sub re_can_move()
        For i As Integer = 0 To 7
            For j As Integer = 0 To 7
                can_move(i, j) = False
            Next
        Next
    End Sub

    Public Sub re_chk_white()
        For i As Integer = 0 To 7
            For j As Integer = 0 To 7
                check_for_white(i, j) = False
            Next
        Next
    End Sub

    Public Sub re_chk_black()
        For i As Integer = 0 To 7
            For j As Integer = 0 To 7
                check_for_black(i, j) = False
            Next
        Next
    End Sub
    'schimba cand o piesa  mananca regele pentru negre
    Public Sub fill_chk_for_black()
        re_chk_black()
        For i As Integer = 0 To 7
            GameForm.w_pawn(i).fill_chk_king()
            For j As Integer = 0 To 7
                For k As Integer = 0 To 7
                    If GameForm.w_pawn(i).check_the_king(j, k) = True Then
                        check_for_black(j, k) = True
                    End If
                Next
            Next
        Next

        For i As Integer = 0 To 1
            GameForm.w_rook(i).fill_chk_king()
            For j As Integer = 0 To 7
                For k As Integer = 0 To 7
                    If GameForm.w_rook(i).check_the_king(j, k) = True Then
                        check_for_black(j, k) = True
                    End If
                Next
            Next
        Next
        For i As Integer = 0 To 1
            GameForm.w_bishop(i).fill_chk_king()
            For j As Integer = 0 To 7
                For k As Integer = 0 To 7
                    If GameForm.w_bishop(i).check_the_king(j, k) = True Then
                        check_for_black(j, k) = True
                    End If
                Next
            Next
        Next

        For i As Integer = 0 To 1
            GameForm.w_knight(i).fill_chk_king()
            For j As Integer = 0 To 7
                For k As Integer = 0 To 7
                    If GameForm.w_knight(i).check_the_king(j, k) = True Then
                        check_for_black(j, k) = True
                    End If
                Next
            Next
        Next

        GameForm.w_queen.fill_chk_king()
        For j As Integer = 0 To 7
            For k As Integer = 0 To 7
                If GameForm.w_queen.check_the_king(j, k) = True Then
                    check_for_black(j, k) = True
                End If
            Next
        Next

    End Sub

    Public Sub fill_chk_for_white()
        re_chk_white()
        For i As Integer = 0 To 7
            GameForm.b_pawn(i).fill_chk_king()
            For j As Integer = 0 To 7
                For k As Integer = 0 To 7
                    If GameForm.b_pawn(i).check_the_king(j, k) = True Then
                        check_for_white(j, k) = True
                    End If
                Next
            Next
        Next

        For i As Integer = 0 To 1
            GameForm.b_rook(i).fill_chk_king()
            For j As Integer = 0 To 7
                For k As Integer = 0 To 7
                    If GameForm.b_rook(i).check_the_king(j, k) = True Then
                        check_for_white(j, k) = True
                    End If
                Next
            Next
        Next
        For i As Integer = 0 To 1
            GameForm.b_bishop(i).fill_chk_king()
            For j As Integer = 0 To 7
                For k As Integer = 0 To 7
                    If GameForm.b_bishop(i).check_the_king(j, k) = True Then
                        check_for_white(j, k) = True
                    End If
                Next
            Next
        Next

        For i As Integer = 0 To 1
            GameForm.b_knight(i).fill_chk_king()
            For j As Integer = 0 To 7
                For k As Integer = 0 To 7
                    If GameForm.b_knight(i).check_the_king(j, k) = True Then
                        check_for_white(j, k) = True
                    End If
                Next
            Next
        Next

        GameForm.b_queen.fill_chk_king()
        For j As Integer = 0 To 7
            For k As Integer = 0 To 7
                If GameForm.b_queen.check_the_king(j, k) = True Then
                    check_for_white(j, k) = True
                End If
            Next
        Next

    End Sub
    'atunta daca regele are sah
    Public Sub show_chk()
        w_in_danger = False
        b_in_danger = False
        '''''''''''''''''''''''''''''''''''''''''verifica negre'''''''''''''''''''''''''''''''''''
        For i As Integer = 0 To 7
            For j As Integer = 0 To 7
                If check_for_black(i, j) = True And GameForm.b_king.x_pos = i And GameForm.b_king.y_pos = j Then
                    GameForm.pb(i, j).BackgroundImage = My.Resources.check
                    b_in_danger = True
                End If
            Next
        Next

        '''''''''''''''''''''''''''''''''''''''''verifica albe'''''''''''''''''''''''''''''''''''
        For i As Integer = 0 To 7
            For j As Integer = 0 To 7
                If check_for_white(i, j) = True And GameForm.w_king.x_pos = i And GameForm.w_king.y_pos = j Then
                    GameForm.pb(i, j).BackgroundImage = My.Resources.check
                    w_in_danger = True
                End If
            Next
        Next
    End Sub

    Public Sub show_possible_move(ByVal b As Boolean(,))
        For i As Integer = 0 To 7
            For j As Integer = 0 To 7
                If b(i, j) = True Then
                    GameForm.pb(i, j).BackgroundImage = My.Resources.V_Move
                    can_move(i, j) = True
                End If
            Next
        Next
    End Sub

    Public Sub mov(ByVal selected, ByVal x, ByVal y, ByVal pre_x, ByVal pre_y)
        Select Case selected
            Case "w_king"
                GameForm.w_king.change_pos(x, y, pre_x, pre_y)
                re_chk_white()
                re_chk_black()
                fill_chk_for_white()
                fill_chk_for_black()
                show_chk()
                player = 2
                chk_winner(selected, x, y)
            Case "w_queen"
                GameForm.w_queen.change_pos(x, y, pre_x, pre_y)
                re_chk_white()
                re_chk_black()
                fill_chk_for_white()
                fill_chk_for_black()
                show_chk()
                player = 2
                chk_winner(selected, x, y)
            Case "w_bishop"
                For i As Integer = 0 To 1
                    If GameForm.w_bishop(i).ret_x = pre_x And GameForm.w_bishop(i).ret_y = pre_y Then
                        GameForm.w_bishop(i).change_pos(x, y, pre_x, pre_y)
                    End If
                Next
                re_chk_white()
                re_chk_black()
                fill_chk_for_white()
                fill_chk_for_black()
                show_chk()
                player = 2
                chk_winner(selected, x, y)
            Case "w_knight"
                For i As Integer = 0 To 1
                    If GameForm.w_knight(i).ret_x = pre_x And GameForm.w_knight(i).ret_y = pre_y Then
                        GameForm.w_knight(i).change_pos(x, y, pre_x, pre_y)
                    End If
                Next
                re_chk_white()
                re_chk_black()
                fill_chk_for_white()
                fill_chk_for_black()
                show_chk()
                player = 2
                chk_winner(selected, x, y)
            Case "w_rook"
                For i As Integer = 0 To 1
                    If GameForm.w_rook(i).ret_x = pre_x And GameForm.w_rook(i).ret_y = pre_y Then
                        GameForm.w_rook(i).change_pos(x, y, pre_x, pre_y)
                    End If
                Next
                re_chk_white()
                re_chk_black()
                fill_chk_for_white()
                fill_chk_for_black()
                show_chk()
                player = 2
                chk_winner(selected, x, y)
            Case "w_pawn"
                For i As Integer = 0 To 7
                    If GameForm.w_pawn(i).ret_x = pre_x And GameForm.w_pawn(i).ret_y = pre_y And board(x, y) <= 0 Then
                        GameForm.w_pawn(i).change_pos(x, y, pre_x, pre_y)
                    End If
                Next
                re_chk_white()
                re_chk_black()
                fill_chk_for_white()
                fill_chk_for_black()
                show_chk()
                player = 2
                chk_winner(selected, x, y)

            Case "b_king"
                GameForm.b_king.change_pos(x, y, pre_x, pre_y)
                re_chk_white()
                re_chk_black()
                fill_chk_for_white()
                fill_chk_for_black()
                show_chk()
                player = 1
                chk_winner(selected, x, y)
            Case "b_queen"
                GameForm.b_queen.change_pos(x, y, pre_x, pre_y)
                re_chk_white()
                re_chk_black()
                fill_chk_for_white()
                fill_chk_for_black()
                show_chk()
                player = 1
                chk_winner(selected, x, y)
            Case "b_bishop"
                For i As Integer = 0 To 1
                    If GameForm.b_bishop(i).ret_x = pre_x And GameForm.b_bishop(i).ret_y = pre_y Then
                        GameForm.b_bishop(i).change_pos(x, y, pre_x, pre_y)
                    End If
                Next
                re_chk_white()
                re_chk_black()
                fill_chk_for_white()
                fill_chk_for_black()
                show_chk()
                player = 1
                chk_winner(selected, x, y)
            Case "b_knight"
                For i As Integer = 0 To 1
                    If GameForm.b_knight(i).ret_x = pre_x And GameForm.b_knight(i).ret_y = pre_y Then
                        GameForm.b_knight(i).change_pos(x, y, pre_x, pre_y)
                    End If
                Next
                re_chk_white()
                re_chk_black()
                fill_chk_for_white()
                fill_chk_for_black()
                show_chk()
                player = 1
                chk_winner(selected, x, y)
            Case "b_rook"
                For i As Integer = 0 To 1
                    If GameForm.b_rook(i).ret_x = pre_x And GameForm.b_rook(i).ret_y = pre_y Then
                        GameForm.b_rook(i).change_pos(x, y, pre_x, pre_y)
                    End If
                Next
                re_chk_white()
                re_chk_black()
                fill_chk_for_white()
                fill_chk_for_black()
                show_chk()
                player = 1
                chk_winner(selected, x, y)
            Case "b_pawn"
                For i As Integer = 0 To 7
                    If GameForm.b_pawn(i).ret_x = pre_x And GameForm.b_pawn(i).ret_y = pre_y And board(x, y) >= 0 Then
                        GameForm.b_pawn(i).change_pos(x, y, pre_x, pre_y)
                    End If
                Next
                re_chk_white()
                re_chk_black()
                fill_chk_for_white()
                fill_chk_for_black()
                show_chk()
                player = 1
                chk_winner(selected, x, y)
            Case Else

        End Select


    End Sub

End Class
