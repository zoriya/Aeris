import { Octokit } from "@octokit/rest";

export class GitHub {
	static commentPR(params: any) {
		var octokit = new Octokit();
		var expectedParameters = ['owner', 'repo', 'pull_number', 'body']
		expectedParameters.forEach((param, _, __) => {
			if (!(param in params))
				throw new MissingParameterException(param);
		});
		octokit.pulls.createReviewComment(params['owner'], params['repo'], params['pull_number'], params['body'], bod);

	}
}