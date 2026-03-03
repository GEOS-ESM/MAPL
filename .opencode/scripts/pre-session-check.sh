#!/bin/bash
echo "=== Pre-Session Verification ==="
echo "Current branch: $(git branch --show-current)"
echo ""
echo "Existing PRs for this branch:"
gh pr list --head $(git branch --show-current)
echo ""
echo "Git status:"
git status --short
echo ""
read -p "Create NEW branch for this session? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    read -p "Enter new branch name (feature/#ISSUE-taskN-desc): " branch_name
    git checkout -b "$branch_name"
    echo "Created and switched to: $branch_name"
fi
