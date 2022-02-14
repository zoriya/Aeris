import { Octokit } from "@octokit/rest";

export class GitHub {

	private static getGitHubInstance() {
		///TODO Get various credentials
		return new Octokit();
	}

	static openPR(params: any) {
		checkParams(params, ['owner', 'repo', 'title', 'head', 'base']);

		this.getGitHubInstance().pulls.create({
			owner: params['owner'], repo: params['repo'], 
			title: params['title'], base: params['base'], head: params['head']
		});
	}

	static commentPR(params: any) {
		checkParams(params, ['owner', 'repo', 'pull_number', 'body']);

		this.getGitHubInstance().pulls.createReviewComment({
			owner: params['owner'], repo: params['repo'], 
			pull_number: params['pull_number'], body: params['body']
		});
	}

	static closePR(params: any) {
		checkParams(params, ['owner', 'repo', 'pull_number']);

		this.getGitHubInstance().pulls.update({
			owner: params['owner'], repo: params['repo'], 
			pull_number: params['pull_number'], state: "closed"
		});
	}

	static mergePR(params: any) {
		checkParams(params, ['owner', 'repo', 'pull_number']);

		this.getGitHubInstance().pulls.merge({
			owner: params['owner'], repo: params['repo'], 
			pull_number: params['pull_number']
		});
	}

	static createIssue(params: any) {
		checkParams(params, ['owner', 'repo', 'title', 'body']);
		
		this.getGitHubInstance().issues.create({
			owner: params['owner'], repo: params['repo'], 
			title: params['title'], body: params['body']
		});
	}
	
	static commentIssue(params: any) {
		checkParams(params, ['owner', 'repo', 'issue_number', 'body']);

		this.getGitHubInstance().issues.createComment({
			owner: params['owner'], repo: params['repo'], 
			issue_number: params['issue_number'], body: params['body']
		});
	}

	static closeIssue(params: any) {
		checkParams(params, ['owner', 'repo', 'issue_number']);

		this.getGitHubInstance().issues.update({
			owner: params['owner'], repo: params['repo'], 
			issue_number: params['issue_number'], state: 'closed'
		});
	}

	static forkRepo(params: any) {
		checkParams(params, ['owner', 'repo']);

		this.getGitHubInstance().repos.createFork({
			owner: params['owner'], repo: params['repo']
		});
	}

	static starRepo(params: any) {
		checkParams(params, ['owner', 'repo']);

		this.getGitHubInstance().activity.starRepoForAuthenticatedUser({
			owner: params['owner'], repo: params['repo']
		});
	}

	static watchRepo(params: any) {
		checkParams(params, ['owner', 'repo']);

		this.getGitHubInstance().activity.setRepoSubscription({
			owner: params['owner'], repo: params['repo']
		});
	}
}