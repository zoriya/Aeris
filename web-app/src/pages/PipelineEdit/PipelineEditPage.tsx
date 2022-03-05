import { AppAREAType, AppPipelineInfoType, AppPipelineType, AppServiceType } from "../../utils/types";
import { lintPipeline } from "../../utils/utils";
import { useState } from "react";
import PipelineModal from "../../components/Pipelines/PipelineModal";

import PipelineEditPipeline from "./PipelineEditPipeline";
import PipelineEditAREA from "./PipelineEditAREA";

interface PipelineEditProps {
	pipelineData: AppPipelineType;
	handleSave: (pD: AppPipelineType) => any;
	handleDelete: (pD: AppPipelineType) => any;
	services: Array<AppServiceType>;
	disableDeletion: boolean;
	actions: Array<AppAREAType>;
	reactions: Array<AppAREAType>;

	handleQuit: () => void;
}

export enum PipelineEditMode {
	Pipeline,
	Action,
	Reactions,
	EditAction,
	EditReaction,
}

export default function PipelineEditPage({
	pipelineData,
	handleSave,
	handleDelete,
	services,
	actions,
	reactions,
	disableDeletion,
	handleQuit,
}: PipelineEditProps) {
	const [mode, setMode] = useState<PipelineEditMode>(PipelineEditMode.Pipeline);
	const [editPipelineData, setEditPipelineData] = useState<AppPipelineType>(pipelineData);
	const [editActionData, setEditActionData] = useState<AppAREAType>(pipelineData.action);
	const [editReactionData, setEditReactionData] = useState<AppAREAType | undefined>();
	const [editReactionIndex, setEditReactionIndex] = useState<number>(0);

	const changeEditPipeline = (pD: AppPipelineType) => setEditPipelineData(lintPipeline(pD, services));

	switch (mode) {
		default:
		case PipelineEditMode.Pipeline:
			return (
				<PipelineEditPipeline
					disableDeletion={disableDeletion}
					handleEditReactionOrder={(new_reas) =>
						changeEditPipeline({
							...editPipelineData,
							reactions: new_reas,
						})
					}
					pipelineData={editPipelineData}
					handleEditPipelineTitle={(newTtitle) =>
						changeEditPipeline({
							...editPipelineData,
							name: newTtitle,
						})
					}
					handleEditPipelineMetaData={(name, enabled) => {
						changeEditPipeline({
							...editPipelineData,
							name: name,
							data: {
								...editPipelineData.data,
								enabled: enabled,
							},
						});
					}}
					handleEditAction={(action) => {
						setEditActionData(action);
						setMode(PipelineEditMode.EditAction);
					}}
					handleEditReaction={(reaction, index) => {
						setEditReactionIndex(index);
						setEditReactionData(reaction);
						setMode(PipelineEditMode.EditReaction);
					}}
					handleDeleteReaction={(reaction, index) => {
						let reactionsTmp = editPipelineData.reactions;
						reactionsTmp.splice(index, 1);
						changeEditPipeline({
							...editPipelineData,
							reactions: reactionsTmp,
						});
					}}
					setEditMode={setMode}
					handleSave={handleSave}
					handleDelete={handleDelete}
					setEditReactionIndex={setEditReactionIndex}
				/>
			);
		case PipelineEditMode.Action:
			return (
				<PipelineModal
					isOpen={true}
					handleClose={() => {
						setMode(PipelineEditMode.Pipeline);
					}}>
					<PipelineEditAREA
						pipelineData={editPipelineData}
						setEditMode={setMode}
						isActions={true}
						setAREA={(AREA: AppAREAType) => {
							changeEditPipeline({
								...editPipelineData,
								action: AREA,
							});
							setMode(PipelineEditMode.Pipeline);
						}}
						services={services}
						AREAs={actions}
					/>
				</PipelineModal>
			);
		case PipelineEditMode.Reactions:
			return (
				<PipelineModal
					isOpen={true}
					handleClose={() => {
						setMode(PipelineEditMode.Pipeline);
					}}>
					<PipelineEditAREA
						pipelineData={editPipelineData}
						setEditMode={setMode}
						isActions={false}
						setAREA={(AREA: AppAREAType) => {
							let reactionsTmp = editPipelineData.reactions;
							reactionsTmp[editReactionIndex] = AREA;
							changeEditPipeline({
								...editPipelineData,
								reactions: reactionsTmp,
							});
							setMode(PipelineEditMode.Pipeline);
						}}
						services={services}
						AREAs={reactions}
					/>
				</PipelineModal>
			);
		case PipelineEditMode.EditAction:
			return (
				<PipelineModal
					isOpen={true}
					handleClose={() => {
						setMode(PipelineEditMode.Pipeline);
					}}>
					<PipelineEditAREA
						pipelineData={editPipelineData}
						setEditMode={setMode}
						isActions={true}
						setAREA={(AREA: AppAREAType) => {
							changeEditPipeline({
								...editPipelineData,
								action: AREA,
							});
							setMode(PipelineEditMode.Pipeline);
						}}
						selectedAREA={editActionData}
						services={services}
						AREAs={actions}
					/>
				</PipelineModal>
			);
		case PipelineEditMode.EditReaction:
			return (
				<PipelineModal
					isOpen={true}
					handleClose={() => {
						setMode(PipelineEditMode.Pipeline);
					}}>
					<PipelineEditAREA
						pipelineData={editPipelineData}
						setEditMode={setMode}
						isActions={false}
						setAREA={(AREA: AppAREAType) => {
							let reactionsTmp = editPipelineData.reactions;
							reactionsTmp[editReactionIndex] = AREA;
							changeEditPipeline({
								...editPipelineData,
								reactions: reactionsTmp,
							});
							setMode(PipelineEditMode.Pipeline);
						}}
						selectedAREA={editReactionData}
						services={services}
						AREAs={reactions}
					/>
				</PipelineModal>
			);
	}
}
