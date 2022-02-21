import 'package:aeris/src/models/reaction.dart';
import 'package:flutter/widgets.dart';
import 'package:reorderables/reorderables.dart';

class ReorderableReactionCardsList extends StatefulWidget {
  ReorderableReactionCardsList(
      {Key? key,
      required this.onReorder,
      required this.reactionList,
      required this.itemBuilder})
      : super(key: key);

  List<Reaction> reactionList;

  // Callback when a list has been reordered
  final void Function() onReorder;

  final Widget Function(Reaction) itemBuilder;

  @override
  State<ReorderableReactionCardsList> createState() =>
      _ReorderableReactionCardsListState();
}

class _ReorderableReactionCardsListState
    extends State<ReorderableReactionCardsList> {
  @override
  Widget build(BuildContext context) => ReorderableColumn(
        ignorePrimaryScrollController: true,
        onReorder: (int oldItemIndex, int newItemIndex) {
          setState(() {
            var movedItem = widget.reactionList.removeAt(oldItemIndex);
            if (newItemIndex > widget.reactionList.length) {
              newItemIndex = widget.reactionList.length;
            }
            widget.reactionList.insert(newItemIndex, movedItem);
            widget.onReorder();
          });
        },
        children: [
          ...[
            for (var reaction in widget.reactionList)
             widget.itemBuilder(reaction)
          ]
        ],
      );
}
