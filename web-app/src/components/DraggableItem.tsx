import React, { useRef } from "react";

export interface DraggableItemProps {
	index: number;
	moveItem: any;
	children: React.ReactNode;
}

/*

export const DraggableItem = ({ children, index, moveItem }: DraggableItemProps) => {
	// useDrag - the list item is draggable
	const [{ isDragging }, dragRef] = useDrag({
		type: "item",
		item: { index },
		collect: (monitor) => ({
			isDragging: monitor.isDragging(),
		}),
	});

	// useDrop - the list item is also a drop area
	const [spec, dropRef] = useDrop({
		accept: "item",
		hover: (item, monitor) => {
			const dragIndex = item.index;
			const hoverIndex = index;
			const hoverBoundingRect = ref.current?.getBoundingClientRect();
			const hoverMiddleY = (hoverBoundingRect.bottom - hoverBoundingRect.top) / 2;
			const hoverActualY = monitor.getClientOffset().y - hoverBoundingRect.top;

			// if dragging down, continue only when hover is smaller than middle Y
			if (dragIndex < hoverIndex && hoverActualY < hoverMiddleY) return;
			// if dragging up, continue only when hover is bigger than middle Y
			if (dragIndex > hoverIndex && hoverActualY > hoverMiddleY) return;

			moveListItem(dragIndex, hoverIndex);
			item.index = hoverIndex;
		},
	});

	// Join the 2 refs together into one (both draggable and can be dropped on)
	const ref = useRef(null);
	const dragDropRef = dragRef(dropRef(ref));

	// Make items being dragged transparent, so it's easier to see where we drop them
	const opacity = isDragging ? 0 : 1;
	return (
		<div ref={dragDropRef} style={{ ...style, opacity }}>
			{text}
		</div>
	);
};

*/