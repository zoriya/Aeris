import { Octokit } from "@octokit/rest";
import { Pipeline, PipelineType, ReactionType, ServiceType } from "../models/pipeline";
import { BaseService, reaction, service } from "../models/base-service";

@service(ServiceType.Github)
export class GitHub extends BaseService {

	private _github: Octokit;

	constructor(_: Pipeline) {
		super();
		///TODO Get various credentials
		this._github = new Octokit();
	}

	@reaction(ReactionType.openPR, ['owner', 'repo', 'title', 'head', 'base'])
	openPR(params: any) {
		this._github.pulls.create({
			owner: params['owner'], repo: params['repo'], 
			title: params['title'], base: params['base'], head: params['head']
		});
	}

	@reaction(ReactionType.commentPR, ['owner', 'repo', 'pull_number', 'body'])
	commentPR(params: any) {
		this._github.pulls.createReviewComment({
			owner: params['owner'], repo: params['repo'], 
			pull_number: params['pull_number'], body: params['body']
		});
	}

	@reaction(ReactionType.closePR, ['owner', 'repo', 'pull_number'])
	closePR(params: any) {
		this._github.pulls.update({
			owner: params['owner'], repo: params['repo'], 
			pull_number: params['pull_number'], state: "closed"
		});
	}

	@reaction(ReactionType.mergePR, ['owner', 'repo', 'pull_number'])
	mergePR(params: any) {
		this._github.pulls.merge({
			owner: params['owner'], repo: params['repo'], 
			pull_number: params['pull_number']
		});
	}

	@reaction(ReactionType.createIssue, ['owner', 'repo', 'title', 'body'])
	createIssue(params: any) {
		this._github.issues.create({
			owner: params['owner'], repo: params['repo'], 
			title: params['title'], body: params['body']
		});
	}
	
	@reaction(ReactionType.commentIssue, ['owner', 'repo', 'issue_number', 'body'])
	commentIssue(params: any) {
		this._github.issues.createComment({
			owner: params['owner'], repo: params['repo'], 
			issue_number: params['issue_number'], body: params['body']
		});
	}

	@reaction(ReactionType.closeIssue, ['owner', 'repo', 'issue_number'])
	closeIssue(params: any) {
		this._github.issues.update({
			owner: params['owner'], repo: params['repo'], 
			issue_number: params['issue_number'], state: 'closed'
		});
	}

	@reaction(ReactionType.createRepo, ['name'])
	createRepo(params: any) {
		this._github.rest.repos.createForAuthenticatedUser({
			name: params['name']
		});
	}

	@reaction(ReactionType.createPrivateRepo, ['name'])
	createPrivateRepo(params: any) {
		this._github.rest.repos.createForAuthenticatedUser({
			name: params['name'], private: true
		});
	}

	@reaction(ReactionType.updateDescription, ['owner', 'repo', 'description'])
	updateDescription(params:any) {
		this._github.repos.update({
			owner: params['owner'], repo: params['repo'],
			description: params['description']
		});
	}

	@reaction(ReactionType.forkRepo, ['owner', 'repo'])
	forkRepo(params: any) {
		this._github.repos.createFork({
			owner: params['owner'], repo: params['repo']
		});
	}

	@reaction(ReactionType.starRepo, ['owner', 'repo'])
	starRepo(params: any) {
		this._github.activity.starRepoForAuthenticatedUser({
			owner: params['owner'], repo: params['repo']
		});
	}

	@reaction(ReactionType.watchRepo, ['owner', 'repo'])
	watchRepo(params: any) {
		this._github.activity.setRepoSubscription({
			owner: params['owner'], repo: params['repo']
		});
	}
}