import 'package:flutter/material.dart';
import 'package:mobile/src/models/pipeline_collection.dart';
import 'package:mobile/src/providers/pipelines_provider.dart';
import 'package:mobile/src/widgets/aeris_popup_menu.dart';
import 'package:mobile/src/widgets/aeris_popup_menu_item.dart';
import 'package:recase/recase.dart';

/// Sorting Menu for the Home Page
class HomePageSortMenu extends StatelessWidget {
  /// Collection to sort
  final PipelineProvider collectionProvider;

  const HomePageSortMenu({Key? key, required this.collectionProvider})
      : super(key: key);

  IconData sortMethodGetIcon(PipelineCollectionSort sortingMethod) {
    switch (sortingMethod) {
      case PipelineCollectionSort.last:
        return Icons.schedule;
      case PipelineCollectionSort.triggerCount:
        return Icons.trending_up;
      case PipelineCollectionSort.name:
        return Icons.text_rotate_vertical;
      case PipelineCollectionSort.triggeringService:
        return Icons.webhook;
    }
  }

  @override
  Widget build(BuildContext context) {
    return AerisPopupMenu(
      itemBuilder: (context) => [
        ...[
          for (var sortingMethod in PipelineCollectionSort.values)
            AerisPopupMenuItem(
              context: context,
              icon: sortMethodGetIcon(sortingMethod),
              title: ReCase(sortingMethod.name).titleCase,
              value: sortingMethod
            ),
        ],
        AerisPopupMenuItem(
          context: context,
          icon: Icons.call_merge,
          title: collectionProvider.pipelineCollection.sortingSplitDisabled
               ///TODO translate
              ? "Merge disabled pipelines"
              : "Seperate disabled pipelines",
          value: ""
        ),
      ],
      onSelected: (sortingMethod) {
        /// TODO: not clean
        if (sortingMethod == "") {
          collectionProvider.pipelineCollection.sortingSplitDisabled = !collectionProvider.pipelineCollection.sortingSplitDisabled;
        } else {
          collectionProvider.pipelineCollection.sortingMethod = sortingMethod as PipelineCollectionSort;
        }
        collectionProvider.sortPipelines();
      },
      icon: Icons.sort,
      menuOffset: const Offset(0, 50),
    );
  }
}
