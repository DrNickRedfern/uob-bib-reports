$destination = $PSScriptRoot + '\report'

If(!(test-path -PathType container $destination))
{
      New-Item -ItemType Directory -Path $destination
}

Get-childitem -recurse -path $PSScriptRoot | 
Where-Object {($_.Name -like '*report*files' -or $_.Name -like '*report*.pdf' -or $_.Name -like '*report*.log')} | 
Move-Item -Destination $destination