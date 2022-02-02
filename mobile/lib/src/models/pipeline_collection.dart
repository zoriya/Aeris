import 'package:flutter/material.dart';
import 'package:mobile/src/models/pipeline.dart';

///
enum PipelineCollectionSort { last, triggerCount, name, triggeringService }

/// Collection of pipelines, to sort them
class PipelineCollection {
  /// The pipelines
  List<Pipeline> pipelines;

  PipelineCollection({Key? key, required this.pipelines});

  /// Sorts the pipeline, returns a reference to list
  List<Pipeline> sort(
      {required PipelineCollectionSort sortingMethod,
      bool splitDisabled = true}) {
    List<Pipeline> enabled = [];
    List<Pipeline> disabled = [];
    int Function(Pipeline, Pipeline) sortingFunction;

    enabled = pipelines.where((pipeline) => pipeline.enabled).toList();
    disabled =
        pipelines.where((pipeline) => pipeline.enabled == false).toList();
    if (splitDisabled) {
      enabled.addAll(disabled);
      disabled.clear();
    }
    switch (sortingMethod) {
      case PipelineCollectionSort.last:
        sortingFunction = (a, b) => b.trigger.last.compareTo(a.trigger.last);
        break;
      case PipelineCollectionSort.triggerCount:
        sortingFunction = (a, b) => b.triggerCount - a.triggerCount;
        break;
      case PipelineCollectionSort.name:
        sortingFunction = (a, b) => b.name.compareTo(a.name);
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
