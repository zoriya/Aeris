import { Octokit } from "@octokit/rest";
import { Pipeline, PipelineEnv, PipelineType, ReactionType, ServiceType } from "../models/pipeline";
import { BaseService, reaction, service } from "../models/base-service";

@service(ServiceType.Github)
export class Github extends BaseService {

	private _github: Octokit;

	constructor(_: Pipeline) {
		super();
		///TODO Get various credentials
		this._github = new Octokit();
	}

	@reaction(ReactionType.OpenPR, ['owner', 'repo', 'title', 'head', 'base'])
	async openPR(params: any): Promise<PipelineEnv> {
		let res = await this._github.pulls.create({
			owner: params['owner'], repo: params['repo'], 
			title: params['title'], base: params['base'], head: params['head']
		});
		return {
			URL: res.data.url,
		};
	}

	@reaction(ReactionType.CommentPR, ['owner', 'repo', 'pull_number', 'body'])
	async commentPR(params: any): Promise<PipelineEnv> {
		let res = await this._github.pulls.createReviewComment({
			owner: params['owner'], repo: params['repo'], 
			pull_number: params['pull_number'], body: params['body']
		});
		return {
			URL: res.url,
		};
	}

	@reaction(ReactionType.ClosePR, ['owner', 'repo', 'pull_number'])
	async closePR(params: any): Promise<PipelineEnv> {
		await this._github.pulls.update({
			owner: params['owner'], repo: params['repo'], 
			pull_number: params['pull_number'], state: "closed"
		});
		return {};
	}

	@reaction(ReactionType.MergePR, ['owner', 'repo', 'pull_number'])
	async mergePR(params: any): Promise<PipelineEnv> {
		await this._github.pulls.merge({
			owner: params['owner'], repo: params['repo'], 
			pull_number: params['pull_number']
		});
		return {};
	}

	@reaction(ReactionType.CreateIssue, ['owner', 'repo', 'title', 'body'])
	async createIssue(params: any): Promise<PipelineEnv> {
		let res = await this._github.issues.create({
			owner: params['owner'], repo: params['repo'], 
			title: params['title'], body: params['body']
		});
		return {
			URL: res.data.url,
		};
	}
	
	@reaction(ReactionType.CommentIssue, ['owner', 'repo', 'issue_number', 'body'])
	async commentIssue(params: any): Promise<PipelineEnv> {
		let res = await this._github.issues.createComment({
			owner: params['owner'], repo: params['repo'], 
			issue_number: params['issue_number'], body: params['body']
		});
		return {
			URL: res.data.url,
		};
	}

	@reaction(ReactionType.CloseIssue, ['owner', 'repo', 'issue_number'])
	async closeIssue(params: any): Promise<PipelineEnv> {
		let res = await this._github.issues.update({
			owner: params['owner'], repo: params['repo'], 
			issue_number: params['issue_number'], state: 'closed'
		});
		return {
			URL: res.data.url
		};
	}

	@reaction(ReactionType.CreateRepo, ['name'])
	async createRepo(params: any): Promise<PipelineEnv> {
		let res = await this._github.rest.repos.createForAuthenticatedUser({
			name: params['name']
		});
		return {
			URL: res.data.url
		};
	}

	@reaction(ReactionType.CreatePrivateRepo, ['name'])
	async createPrivateRepo(params: any): Promise<PipelineEnv> {
		let res = await this._github.rest.repos.createForAuthenticatedUser({
			name: params['name'], private: true
		});
		return {
			URL: res.data.url
		};
	}

	@reaction(ReactionType.UpdateDescription, ['owner', 'repo', 'description'])
	async updateDescription(params:any): Promise<PipelineEnv> {
		let res = await this._github.repos.update({
			owner: params['owner'], repo: params['repo'],
			description: params['description']
		});
		return {
			URL: res.data.url
		};
	}

	@reaction(ReactionType.ForkRepo, ['owner', 'repo'])
	async forkRepo(params: any): Promise<PipelineEnv> {
		let res = await this._github.repos.createFork({
			owner: params['owner'], repo: params['repo']
		});
		return {
			URL: res.data.url
		};
	}

	@reaction(ReactionType.StarRepo, ['owner', 'repo'])
	async starRepo(params: any): Promise<PipelineEnv> {
		await this._github.activity.starRepoForAuthenticatedUser({
			owner: params['owner'], repo: params['repo']
		});
		return {};
	}

	@reaction(ReactionType.WatchRepo, ['owner', 'repo'])
	async watchRepo(params: any): Promise<PipelineEnv> {
		await this._github.activity.setRepoSubscription({
			owner: params['owner'], repo: params['repo']
		});
		return {};
	}
}
