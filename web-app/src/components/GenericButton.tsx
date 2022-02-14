import { Box, CardContent, Typography, CardMedia, IconButton } from "@mui/material";
import Card from "@mui/material/Card"
import "./GenericButton.css"

import type { ServiceProps } from './types'


interface GenericButtonProps {
    title: string,

    service: ServiceProps

    trailingIcon: JSX.Element
}

export type { GenericButtonProps }

export default function GenericButton({ title, service, trailingIcon }: GenericButtonProps)
{
    return (
        <Card className="GenericButton" sx={{ alignItems:"center", borderRadius:"15px", padding:"10px", width:"500px" }}>
            <Box className="GenericButtonMedia"  sx={{ display:'flex', flex: '1 0 auto', alignItems:"center" }}>
                <CardMedia
                    component="img"
                    sx={{ width: 70 }}
                    image={ service.imageSrc }
                    alt={ service.altText }
                />
            </Box>
            <Box sx={{ flex: '5 0 auto' }}>
                <CardContent>
                    <Typography variant="h4" noWrap align="center" width={"300px"} >
                        { title }
                    </Typography>
                </CardContent>
            </Box>
            <Box sx={{ flex: '1 0 auto' }}>
                <IconButton color="secondary" aria-label="Options" component="span" >
                    { trailingIcon }
                </IconButton>
            </Box>
		</Card>
    );
}
