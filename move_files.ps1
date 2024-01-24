# python html_redirect.py

$destination = $PSScriptRoot + '\report'

If(!(test-path -PathType container $destination))
{
      New-Item -ItemType Directory -Path $destination
}

Get-childitem -recurse -path $PSScriptRoot | 
Where-Object {($_.Name -like '*report*files' -or $_.Name -like '*report*cache' -or $_.Name -like '*report*.html' -or $_.Name -like '*report*.pdf' -or $_.Name -like '*report*.log' -or $_.Name -like 'custom.css')} | 
Move-Item -Destination $destination