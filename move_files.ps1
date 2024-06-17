# The purpose of the code is to organize and consolidate report-related files 
# in a single directory for better management and accessibility.
# The code does not take any direct input from the user. However, it relies 
# on the current working directory ($PSScriptRoot) to determine the location 
# of the files to be moved.
# The output of the code is the creation of a new directory named "report" 
# (if it doesn't already exist) and the relocation of specific files matching 
# certain naming patterns into that directory.
$destination = $PSScriptRoot + '\report'

If(!(test-path -PathType container $destination))
{
      New-Item -ItemType Directory -Path $destination
}

Get-childitem -recurse -path $PSScriptRoot | 
Where-Object {($_.Name -like '*report*files' -or $_.Name -like '*report*cache' -or $_.Name -like '*report*.html' -or $_.Name -like '*report*.pdf' -or $_.Name -like '*report*.log' -or $_.Name -like 'custom.css')} | 
Move-Item -Destination $destination