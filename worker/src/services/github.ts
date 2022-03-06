import { Octokit } from "@octokit/rest";
import { createOAuthAppAuth, createOAuthUserAuth } from  "@octokit/auth-oauth-app"
import { Pipeline, PipelineEnv, PipelineType, ReactionType, ServiceType } from "../models/pipeline";
import { action, BaseService, reaction, service } from "../models/base-service";
import { Webhooks, EmitterWebhookEventName } from "@octokit/webhooks";
import { filter, fromEventPattern, map, Observable } from "rxjs";

@service(ServiceType.Github)
export class Github extends BaseService {

	private _github: Octokit;
	private _websocket: Webhooks;

	constructor(pipeline: Pipeline) {
		super();
		if (!("Github" in pipeline.userData))
			throw new Error("User not authenticated via github");
		this._github = new Octokit({auth: pipeline.userData["Github"].accessToken});
		this._websocket = new Webhooks({
			secret: "bidibi"
		});
	}
	
	private fromGitHubEvent(
		eventName: EmitterWebhookEventName,
		filterMe: (_: any) => boolean,
		mapMe: (_: any) => any
	): Observable<PipelineEnv> {
		return fromEventPattern(
			(h) => this._websocket.on(eventName, h),
			(h) => this._websocket.removeListener(eventName, h)
		).pipe(
			filter(({ _, __, payload }) => filterMe.call(payload)),
			map(({ _, __, payload }) => mapMe(payload)),
		);
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

	@action(PipelineType.OnOpenPR, ['owner', 'repo'])
	listenOpenPR(params: any): Observable<PipelineEnv> {
		return this.fromGitHubEvent(
			"pull_request.opened",
			(payload) => payload.repository.owner.login == params['owner']
				&& payload.repository.name == params['repo'],
			(payload) => ({
				PR_NAME: payload.pull_request.title,
				PR_BODY: payload.pull_request.body,
				PR_OPENER: payload.sender.login,
				PR_HEAD: payload.head.ref,
				PR_BASE: payload.base.ref,
				REPO_NAME: payload.repository.name,
				REPO_OWNER: payload.repository.owner.login
			})
		);
	}

	@reaction(ReactionType.ClosePR, ['owner', 'repo', 'pull_number'])
	async closePR(params: any): Promise<PipelineEnv> {
		await this._github.pulls.update({
			owner: params['owner'], repo: params['repo'], 
			pull_number: params['pull_number'], state: "closed"
		});
		return {};
	}

	@action(PipelineType.OnClosePR, ['owner', 'repo'])
	listenClosePR(params: any): Observable<PipelineEnv> {
		return this.fromGitHubEvent(
			"pull_request.closed",
			(payload) => payload.repository.owner.login == params['owner']
				&& payload.repository.name == params['repo']
				&& payload.action == "closed" && payload.pull_request.merged == false,
			(payload) => ({
				PR_NAME: payload.pull_request.title,
				PR_BODY: payload.pull_request.body,
				PR_OPENER: payload.sender.login,
				PR_HEAD: payload.head.ref,
				PR_BASE: payload.base.ref,
				REPO_NAME: payload.repository.name,
				REPO_OWNER: payload.repository.owner.login
			})
		);
	}

	@reaction(ReactionType.MergePR, ['owner', 'repo', 'pull_number'])
	async mergePR(params: any): Promise<PipelineEnv> {
		await this._github.pulls.merge({
			owner: params['owner'], repo: params['repo'], 
			pull_number: params['pull_number']
		});
		return {};
	}

	@action(PipelineType.OnMergePR, ['owner', 'repo'])
	listenMergePR(params: any): Observable<PipelineEnv> {
		return this.fromGitHubEvent(
			"pull_request.edited",
			(payload) => payload.repository.owner.login == params['owner']
				&& payload.repository.name == params['repo']
				&& payload.action == "closed" && payload.pull_request.merged,
			(payload) => ({
				PR_NAME: payload.pull_request.title,
				PR_BODY: payload.pull_request.body,
				PR_OPENER: payload.sender.login,
				PR_HEAD: payload.head.ref,
				PR_BASE: payload.base.ref,
				REPO_NAME: payload.repository.name,
				REPO_OWNER: payload.repository.owner.login
			})
		);
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

	@action(PipelineType.OnCreateIssue, ['owner', 'repo'])
	listenOnCreateIssue(params: any): Observable<PipelineEnv> {
		return this.fromGitHubEvent(
			"issues.opened",
			(payload) => payload.repository.owner.login == params['owner'] 
				&& payload.repository.name == params['repo'],
			(payload) => ({
				REPO_NAME: payload.repository.name,
				REPO_OWNER: payload.repository.owner.login,
				ISSUE_NAME: payload.issue.title,
				ISSUE_CONTENT: payload.issue.body,
				ISSUE_AUTHOR: payload.sender.login,
			})
		);
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

	@action(PipelineType.OnCommentIssue, ['owner', 'repo'])
	listenOnCommentIssue(params: any): Observable<PipelineEnv> {
		return this.fromGitHubEvent(
			"issue_comment",
			(payload) => payload.repository.owner.login == params['owner'] 
				&& payload.repository.name == params['repo'],
			(payload) => ({
				REPO_NAME: payload.repository.name,
				REPO_OWNER: payload.repository.owner.login,
				ISSUE_NAME: payload.issue.title,
				ISSUE_CONTENT: payload.issue.body,
				ISSUE_AUTHOR: payload.sender.login,
				COMMENT: payload.comment.body,
				COMMENTER: payload.comment.user.login
			})
		);
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

	@action(PipelineType.OnCloseIssue, ['owner', 'repo'])
	listenOnIssueClose(params: any): Observable<PipelineEnv> {
		return this.fromGitHubEvent(
			"issues.opened",
			(payload) => payload.repository.owner.login == params['owner'] 
				&& payload.repository.name == params['repo'],
			(payload) => ({
				REPO_NAME: payload.repository.name,
				REPO_OWNER: payload.repository.owner.login,
				ISSUE_NAME: payload.issue.title,
				ISSUE_CONTENT: payload.issue.body,
				ISSUE_AUTHOR: payload.sender.login
			})
		);
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

	@action(PipelineType.OnForkRepo, ['owner', 'repo'])
	listenOnForkRepo(params: any): Observable<PipelineEnv> {
		return this.fromGitHubEvent(
			"fork",
			(payload) => payload.repository.owner.login == params['owner'] 
				&& payload.repository.name == params['repo'],
			(payload) => ({
				REPO_NAME: payload.repository.name,
				REPO_OWNER: payload.repository.owner.login,
				FORKER: payload.forkee.owner.login,
			})
		);
	}

	@reaction(ReactionType.StarRepo, ['owner', 'repo'])
	async starRepo(params: any): Promise<PipelineEnv> {
		await this._github.activity.starRepoForAuthenticatedUser({
			owner: params['owner'], repo: params['repo']
		});
		return {};
	}

	@action(PipelineType.OnStarRepo, ['owner', 'repo'])
	listenOnStarRepo(params: any): Observable<PipelineEnv> {
		return this.fromGitHubEvent(
			"star.created",
			(payload) => payload.repository.owner.login == params['owner'] 
				&& payload.repository.name == params['repo'],
			(payload) => ({
				REPO_NAME: payload.repository.name,
				REPO_OWNER: payload.repository.owner.login,
				STAR_COUNT: payload.repository.stargazers_count,
				STARER: payload.sender.login
			})
		);
	}

	@reaction(ReactionType.WatchRepo, ['owner', 'repo'])
	async watchRepo(params: any): Promise<PipelineEnv> {
		await this._github.activity.setRepoSubscription({
			owner: params['owner'], repo: params['repo']
		});
		return {};
	}

	@action(PipelineType.OnWatchRepo, ['owner', 'repo'])
	listenOnWatchRepo(params: any): Observable<PipelineEnv> {
		return this.fromGitHubEvent(
			"watch.started",
			(payload) => payload.repository.owner.login == params['owner'] 
				&& payload.repository.name == params['repo'],
			(payload) => ({
				REPO_NAME: payload.repository.name,
				REPO_OWNER: payload.repository.owner.login,
				WATCH_COUNT: payload.repository.watchers,
				WATCHER: payload.sender.login
			})
		);
	}
}
