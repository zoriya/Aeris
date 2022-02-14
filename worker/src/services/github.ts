import { Octokit } from "@octokit/rest";

export class GitHub {

	private static getGitHubInstance() {
		return new Octokit();
	}

	static commentPR(params: any) {
		var expectedParameters = ['owner', 'repo', 'pull_number', 'body']
		expectedParameters.forEach((param, _, __) => {
			if (!(param in params))
				throw new MissingParameterException(param);
		});
		this.getGitHubInstance().pulls.createReviewComment({
			owner: params['owner'], repo: params['repo'], 
			pull_number: params['pull_number'], body: params['body']
		});
	}

	static commentIssue(params: any) {
		var expectedParameters = ['owner', 'repo', 'issue_number', 'body']
		expectedParameters.forEach((param, _, __) => {
			if (!(param in params))
				throw new MissingParameterException(param);
		});
		this.getGitHubInstance().issues.createComment({
			owner: params['owner'], repo: params['repo'], 
			issue_number: params['issue_number'], body: params['body']
		});
	}
}