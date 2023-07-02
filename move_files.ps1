$source = Get-Location
$destination = "$source\report"
Get-childitem -recurse -path "$source" | 
Where-Object {($_.Name -like "*report_files" -or $_.Name -like "*report*.pdf" -or $_.Name -like "*report*.log")} | 
Move-Item -Destination $destination