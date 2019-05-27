const express = require('express');
const bodyParser = require('body-parser');

const app = express();
app.use(bodyParser.json());

app.post('/app/installations/:installation/access_tokens', (req, res) => res.send({
    token: 'mytoken', expires_at: "123"
}));

app.post('/orgs/:org/repos', (req, res) => {
    console.log('posting repo', req.body);
    setTimeout(() =>
        res.send({
            "id": 1296269,
            "node_id": "MDEwOlJlcG9zaXRvcnkxMjk2MjY5",
            "name": "Hello-World",
            "full_name": "octocat/Hello-World",
            "owner": {
                "login": "octocat",
                "id": 1,
                "node_id": "MDQ6VXNlcjE=",
                "avatar_url": "https://github.com/images/error/octocat_happy.gif",
                "gravatar_id": "",
                "url": "https://api.github.com/users/octocat",
                "html_url": "https://github.com/octocat",
                "followers_url": "https://api.github.com/users/octocat/followers",
                "following_url": "https://api.github.com/users/octocat/following{/other_user}",
                "gists_url": "https://api.github.com/users/octocat/gists{/gist_id}",
                "starred_url": "https://api.github.com/users/octocat/starred{/owner}{/repo}",
                "subscriptions_url": "https://api.github.com/users/octocat/subscriptions",
                "organizations_url": "https://api.github.com/users/octocat/orgs",
                "repos_url": "https://api.github.com/users/octocat/repos",
                "events_url": "https://api.github.com/users/octocat/events{/privacy}",
                "received_events_url": "https://api.github.com/users/octocat/received_events",
                "type": "User",
                "site_admin": false
            },
            "private": false,
            "html_url": "https://github.com/octocat/Hello-World",
            "description": "This your first repo!",
            "fork": false,
            "url": "https://api.github.com/repos/octocat/Hello-World",
            "archive_url": "http://api.github.com/repos/octocat/Hello-World/{archive_format}{/ref}",
            "assignees_url": "http://api.github.com/repos/octocat/Hello-World/assignees{/user}",
            "blobs_url": "http://api.github.com/repos/octocat/Hello-World/git/blobs{/sha}",
            "branches_url": "http://api.github.com/repos/octocat/Hello-World/branches{/branch}",
            "collaborators_url": "http://api.github.com/repos/octocat/Hello-World/collaborators{/collaborator}",
            "comments_url": "http://api.github.com/repos/octocat/Hello-World/comments{/number}",
            "commits_url": "http://api.github.com/repos/octocat/Hello-World/commits{/sha}",
            "compare_url": "http://api.github.com/repos/octocat/Hello-World/compare/{base}...{head}",
            "contents_url": "http://api.github.com/repos/octocat/Hello-World/contents/{+path}",
            "contributors_url": "http://api.github.com/repos/octocat/Hello-World/contributors",
            "deployments_url": "http://api.github.com/repos/octocat/Hello-World/deployments",
            "downloads_url": "http://api.github.com/repos/octocat/Hello-World/downloads",
            "events_url": "http://api.github.com/repos/octocat/Hello-World/events",
            "forks_url": "http://api.github.com/repos/octocat/Hello-World/forks",
            "git_commits_url": "http://api.github.com/repos/octocat/Hello-World/git/commits{/sha}",
            "git_refs_url": "http://api.github.com/repos/octocat/Hello-World/git/refs{/sha}",
            "git_tags_url": "http://api.github.com/repos/octocat/Hello-World/git/tags{/sha}",
            "git_url": "git:github.com/octocat/Hello-World.git",
            "issue_comment_url": "http://api.github.com/repos/octocat/Hello-World/issues/comments{/number}",
            "issue_events_url": "http://api.github.com/repos/octocat/Hello-World/issues/events{/number}",
            "issues_url": "http://api.github.com/repos/octocat/Hello-World/issues{/number}",
            "keys_url": "http://api.github.com/repos/octocat/Hello-World/keys{/key_id}",
            "labels_url": "http://api.github.com/repos/octocat/Hello-World/labels{/name}",
            "languages_url": "http://api.github.com/repos/octocat/Hello-World/languages",
            "merges_url": "http://api.github.com/repos/octocat/Hello-World/merges",
            "milestones_url": "http://api.github.com/repos/octocat/Hello-World/milestones{/number}",
            "notifications_url": "http://api.github.com/repos/octocat/Hello-World/notifications{?since,all,participating}",
            "pulls_url": "http://api.github.com/repos/octocat/Hello-World/pulls{/number}",
            "releases_url": "http://api.github.com/repos/octocat/Hello-World/releases{/id}",
            "ssh_url": "git@github.com:octocat/Hello-World.git",
            "stargazers_url": "http://api.github.com/repos/octocat/Hello-World/stargazers",
            "statuses_url": "http://api.github.com/repos/octocat/Hello-World/statuses/{sha}",
            "subscribers_url": "http://api.github.com/repos/octocat/Hello-World/subscribers",
            "subscription_url": "http://api.github.com/repos/octocat/Hello-World/subscription",
            "tags_url": "http://api.github.com/repos/octocat/Hello-World/tags",
            "teams_url": "http://api.github.com/repos/octocat/Hello-World/teams",
            "trees_url": "http://api.github.com/repos/octocat/Hello-World/git/trees{/sha}",
            "clone_url": "https://github.com/octocat/Hello-World.git",
            "mirror_url": "git:git.example.com/octocat/Hello-World",
            "hooks_url": "http://api.github.com/repos/octocat/Hello-World/hooks",
            "svn_url": "https://svn.github.com/octocat/Hello-World",
            "homepage": "https://github.com",
            "language": null,
            "forks_count": 9,
            "stargazers_count": 80,
            "watchers_count": 80,
            "size": 108,
            "default_branch": "master",
            "open_issues_count": 0,
            "topics": [
                "octocat",
                "atom",
                "electron",
                "api"
            ],
            "has_issues": true,
            "has_projects": true,
            "has_wiki": true,
            "has_pages": false,
            "has_downloads": true,
            "archived": false,
            "disabled": false,
            "pushed_at": "2011-01-26T19:06:43Z",
            "created_at": "2011-01-26T19:01:12Z",
            "updated_at": "2011-01-26T19:14:43Z",
            "permissions": {
                "admin": false,
                "push": false,
                "pull": true
            },
            "allow_rebase_merge": true,
            "allow_squash_merge": true,
            "allow_merge_commit": true,
            "subscribers_count": 42,
            "network_count": 0
        }),
    1000);
});

app.get('/teams/:id', (req, res) => {
    console.log('Get team with id', req.params.id);
    res.status(200).send({
        "id": 1,
        "node_id": "MDQ6VGVhbTE=",
        "url": "https://api.github.com/teams/1",
        "name": "Justice League",
        "slug": "justice-league",
        "description": "A great team.",
        "privacy": "closed",
        "permission": "admin",
        "members_url": "https://api.github.com/teams/1/members{/member}",
        "repositories_url": "https://api.github.com/teams/1/repos",
        "parent": null,
        "members_count": 3,
        "repos_count": 10,
        "created_at": "2017-07-14T16:53:42Z",
        "updated_at": "2017-08-17T12:37:15Z",
        "organization": {
            "login": "github",
            "id": 1,
            "node_id": "MDEyOk9yZ2FuaXphdGlvbjE=",
            "url": "https://api.github.com/orgs/github",
            "repos_url": "https://api.github.com/orgs/github/repos",
            "events_url": "https://api.github.com/orgs/github/events",
            "hooks_url": "https://api.github.com/orgs/github/hooks",
            "issues_url": "https://api.github.com/orgs/github/issues",
            "members_url": "https://api.github.com/orgs/github/members{/member}",
            "public_members_url": "https://api.github.com/orgs/github/public_members{/member}",
            "avatar_url": "https://github.com/images/error/octocat_happy.gif",
            "description": "A great organization",
            "name": "github",
            "company": "GitHub",
            "blog": "https://github.com/blog",
            "location": "San Francisco",
            "email": "octocat@github.com",
            "is_verified": true,
            "has_organization_projects": true,
            "has_repository_projects": true,
            "public_repos": 2,
            "public_gists": 1,
            "followers": 20,
            "following": 0,
            "html_url": "https://github.com/octocat",
            "created_at": "2008-01-14T04:33:35Z",
            "type": "Organization"
        }
    });
});

app.put('/teams/:teamId/repos/:owner/:repo', (req, res) => {
    console.log('putting permission', req.body);
    res.status(204).send();
});

app.put('/repos/:owner/:repo/contents/:path', (req, res) => {
    console.log('Adding file', req.body);
    res.status(200).send(
        {
            "content": {
                "name": "hello.txt",
                "path": "notes/hello.txt",
                "sha": "95b966ae1c166bd92f8ae7d1c313e738c731dfc3",
                "size": 9,
                "url": "https://api.github.com/repos/octocat/Hello-World/contents/notes/hello.txt",
                "html_url": "https://github.com/octocat/Hello-World/blob/master/notes/hello.txt",
                "git_url": "https://api.github.com/repos/octocat/Hello-World/git/blobs/95b966ae1c166bd92f8ae7d1c313e738c731dfc3",
                "download_url": "https://raw.githubusercontent.com/octocat/HelloWorld/master/notes/hello.txt",
                "type": "file",
                "_links": {
                    "self": "https://api.github.com/repos/octocat/Hello-World/contents/notes/hello.txt",
                    "git": "https://api.github.com/repos/octocat/Hello-World/git/blobs/95b966ae1c166bd92f8ae7d1c313e738c731dfc3",
                    "html": "https://github.com/octocat/Hello-World/blob/master/notes/hello.txt"
                }
            },
            "commit": {
                "sha": "7638417db6d59f3c431d3e1f261cc637155684cd",
                "node_id": "MDY6Q29tbWl0NzYzODQxN2RiNmQ1OWYzYzQzMWQzZTFmMjYxY2M2MzcxNTU2ODRjZA==",
                "url": "https://api.github.com/repos/octocat/Hello-World/git/commits/7638417db6d59f3c431d3e1f261cc637155684cd",
                "html_url": "https://github.com/octocat/Hello-World/git/commit/7638417db6d59f3c431d3e1f261cc637155684cd",
                "author": {
                    "date": "2014-11-07T22:01:45Z",
                    "name": "Some Author",
                    "email": "someauthor@gmail.com"
                },
                "committer": {
                    "date": "2014-11-07T22:01:45Z",
                    "name": "Some Author",
                    "email": "someauthor@gmail.com"
                },
                "message": "my commit message",
                "tree": {
                    "url": "https://api.github.com/repos/octocat/Hello-World/git/trees/691272480426f78a0138979dd3ce63b77f706feb",
                    "sha": "691272480426f78a0138979dd3ce63b77f706feb"
                },
                "parents": [
                    {
                        "url": "https://api.github.com/repos/octocat/Hello-World/git/commits/1acc419d4d6a9ce985db7be48c6349a0475975b5",
                        "html_url": "https://github.com/octocat/Hello-World/git/commit/1acc419d4d6a9ce985db7be48c6349a0475975b5",
                        "sha": "1acc419d4d6a9ce985db7be48c6349a0475975b5"
                    }
                ],
                "verification": {
                    "verified": false,
                    "reason": "unsigned",
                    "signature": null,
                    "payload": null
                }
            }
        }
    );
});

app.get('/orgs/:org/teams', (req, res) => res.send([
    { "slug":"ai", "name":"AI", "id":2801344, "description":"IT informasjonsplattform AI-LAB" },
    { "slug":"apen-kildekode", "name":"Åpen kildekode", "id":2702249, "description":"Blabla" },
    { "slug":"core", "name":"Core", "id":1422826 },
    { "slug":"dagpenger", "name":"Dagpenger", "id":2639665, "description":"" }
]));

const port = 3002;

app.listen(port, () => console.log(`GitHub API mock listening on port ${port}`));
