import { Octokit } from "@octokit/rest";

export class GitHub {

	private static getGitHubInstance() {
		return new Octokit();
	}

	static commentPR(params: any) {
		checkParams(params, ['owner', 'repo', 'pull_number', 'body']);

		this.getGitHubInstance().pulls.createReviewComment({
			owner: params['owner'], repo: params['repo'], 
			pull_number: params['pull_number'], body: params['body']
		});
	}

	static commentIssue(params: any) {
		checkParams(params, ['owner', 'repo', 'issue_number', 'body']);

		this.getGitHubInstance().issues.createComment({
			owner: params['owner'], repo: params['repo'], 
			issue_number: params['issue_number'], body: params['body']
		});
	}
}