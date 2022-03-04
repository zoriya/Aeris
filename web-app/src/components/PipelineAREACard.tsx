import {
	Card,
	CardHeader,
	CardContent,
	Avatar,
	CardActions,
	IconButton,
	Chip,
	Grid,
	Divider,
	Collapse,
} from "@mui/material";
import { IconButtonProps } from "@mui/material/IconButton";
import ExpandMoreIcon from "@mui/icons-material/ExpandMore";
import DeleteIcon from "@mui/icons-material/Delete";
import EditIcon from "@mui/icons-material/Edit";
import { styled } from "@mui/material/styles";

import { AppAREAType } from "../utils/types";
import { useState } from "react";
import i18next from "i18next";

interface ExpandMoreProps extends IconButtonProps {
	expand: boolean;
}

const ExpandMore = styled((props: ExpandMoreProps) => {
	const { expand, ...other } = props;
	return <IconButton {...other} />;
})(({ theme, expand }) => ({
	transform: !expand ? "rotate(0deg)" : "rotate(180deg)",
	marginLeft: "auto",
	transition: theme.transitions.create("transform", {
		duration: theme.transitions.duration.shortest,
	}),
}));

export interface PipelineAREACardProps {
	AREA: AppAREAType;
	order: number;
	style?: any;
	canBeRemoved: boolean;
	handleEdit: () => any;
	handleDelete: () => any;
	onClick: React.MouseEventHandler<HTMLButtonElement>;
}

export const PipelineAREACard = ({
	AREA,
	order,
	handleDelete,
	handleEdit,
	style,
	onClick,
	canBeRemoved,
}: PipelineAREACardProps) => {
	const [expanded, setExpanded] = useState<boolean>(false);
	const languageUid = i18next.resolvedLanguage;

	return (
		<Card sx={style}>
			<CardHeader
				avatar={
					<Avatar
						style={{ objectFit: "cover", height: "100%" }}
						alt={AREA.service.logo.altText}
						src={AREA.service.logo.imageSrc}
						variant={"square"}
					/>
				}
				action={
					<ExpandMore
						expand={expanded}
						onClick={() => setExpanded(!expanded)}
						aria-expanded={expanded}
						aria-label="show more">
						<ExpandMoreIcon />
					</ExpandMore>
				}
				title={AREA.type}
				subheader={"#" + order}
			/>
			<Collapse in={expanded} timeout="auto" unmountOnExit>
				<CardActions sx={{ display: "flex", justifyContent: "flex-end" }}>
					<IconButton
						disabled={!canBeRemoved}
						onClick={handleDelete}
						aria-label="Delete"
						size="small"
						color="secondary"
						sx={{ float: "left" }}>
						<DeleteIcon />
					</IconButton>
					<IconButton onClick={handleEdit} aria-label="Edit" size="small" color="secondary" sx={{ float: "left" }}>
						<EditIcon />
					</IconButton>
				</CardActions>
				<CardContent>
					<Grid container spacing={1}>
						{Object.entries(AREA.params).map((el, idx) => {
							return (
								<Grid item display={"flex"} justifyContent={"space-between"} width={"100%"} key={idx}>
									<Chip
										label={el[0]}
										title={el[1].description[languageUid]}
										color="secondary"
										variant="outlined"
										size="small"
									/>
									<code>{el[1].value}</code>
								</Grid>
							);
						})}
					</Grid>
					<Divider style={{ margin: "5px 0px" }} />
					<Grid container spacing={0.5}>
						{Object.entries(AREA.returns).map((el, idx) => {
							return (
								<Grid item key={idx}>
									<Chip label={el[0]} title={el[1]} color="primary" variant="outlined" size="small" />
								</Grid>
							);
						})}
					</Grid>
				</CardContent>
			</Collapse>
		</Card>
	);
};
