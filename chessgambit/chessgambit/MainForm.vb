Public Class MainForm

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        If TextBox1.Text = "" And TextBox2.Text = "" Then
            MsgBox("Va rog, introduceti numele jucatorului")
        Else
            GameForm.Show()
        End If
    End Sub
End Class
