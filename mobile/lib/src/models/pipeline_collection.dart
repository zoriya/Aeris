import 'package:flutter/material.dart';
import 'package:mobile/src/models/pipeline.dart';

///
enum PipelineCollectionSort { last, triggerCount, name, triggeringService }

/// Collection of pipelines, to sort them
class PipelineCollection {
  /// The pipelines
  List<Pipeline> pipelines;

  /// Sorting method for posts
  PipelineCollectionSort sortingMethod;

  /// Seperate disabled pipelines from enabled ones
  bool sortingSplitDisabled;

  PipelineCollection(
      {Key? key,
      required this.pipelines,
      required this.sortingMethod,
      required this.sortingSplitDisabled}) {
    sort();
  }

  /// Sorts the pipeline, returns a reference to list
  List<Pipeline> sort() {
    List<Pipeline> enabled = [];
    List<Pipeline> disabled = [];
    int Function(Pipeline, Pipeline) sortingFunction;

    enabled = pipelines.where((pipeline) => pipeline.enabled).toList();
    disabled =
        pipelines.where((pipeline) => pipeline.enabled == false).toList();
    if (sortingSplitDisabled == false) {
      enabled.addAll(disabled);
      disabled.clear();
    }
    switch (sortingMethod) {
      case PipelineCollectionSort.last:
        sortingFunction = (a, b) {
          if (b.trigger.last == null) return -1;
          if (a.trigger.last == null) return 1;
          return b.trigger.last!.compareTo(a.trigger.last!);
        };
        break;
      case PipelineCollectionSort.triggerCount:
        sortingFunction = (a, b) => b.triggerCount - a.triggerCount;
        break;
      case PipelineCollectionSort.name:
        sortingFunction =
            (a, b) => a.name.toLowerCase().compareTo(b.name.toLowerCase());
        break;
      case PipelineCollectionSort.triggeringService:
        sortingFunction = (a, b) => b.trigger.name.compareTo(a.trigger.name);
        break;
    }
    enabled.sort(sortingFunction);
    disabled.sort(sortingFunction);
    pipelines = [...enabled, ...disabled];
    return pipelines;
  }
}
