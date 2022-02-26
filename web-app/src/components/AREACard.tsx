import {
	Card,
	Chip,
	CardActionArea,
	CardMedia,
	CardContent,
	CardHeader,
	Typography,
	Stack,
	Avatar,
	Divider,
	Box,
	Grid,
} from "@mui/material";

import { AppAREAType } from "../utils/types";

export interface AREACardProps {
	AREA: AppAREAType;
	onClick?: React.MouseEventHandler<HTMLButtonElement>;
}

export const AREACard = ({ AREA, onClick }: AREACardProps) => {
	return (
		<Card sx={{ width: "500px" }}>
			<CardActionArea onClick={onClick}>
				<CardHeader
					avatar={<Avatar style={{ objectFit: 'cover', height: '100%',}} alt={AREA.service.logo.altText} src={AREA.service.logo.imageSrc} variant={"square"} />}
					title={<Typography variant="h5">{AREA.type}</Typography>}
					subheader={AREA.description}
				/>
				{Object.keys(AREA.params.contents).length > 0 || Object.keys(AREA.returns).length > 0 ? (
					<CardContent>
						<Grid container spacing={1} marginBottom={"5px"}>
							{Object.entries(AREA.params.contents).map((el, idx) => {
								return (
									<Grid item key={idx}>
										<Chip label={el[0]} title={el[1].description} color="secondary" variant="outlined" size="small" />
									</Grid>
								);
							})}
						</Grid>

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
				) : (
					<div></div>
				)}
			</CardActionArea>
		</Card>
	);
};
