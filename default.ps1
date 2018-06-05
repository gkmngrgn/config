Import-Module posh-git

function prompt {
    $prompt = & $GitPromptScriptBlock
    $prompt += Write-Prompt "`r`n$ " -ForegroundColor Green
    if ($prompt) {$prompt} else {" "}
}
