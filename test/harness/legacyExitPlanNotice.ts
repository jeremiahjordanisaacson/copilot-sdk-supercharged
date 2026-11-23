/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *--------------------------------------------------------------------------------------------*/

export function isLegacyExitPlanNotice(body: string): boolean {
  const delta =
    /^(?:New tools available|Tools no longer available): exit_plan_mode$/;
  const parts = body
    .trim()
    .split(/\r?\n\s*\r?\n/)
    .map((part) => part.trim());
  return (
    parts.some((part) => delta.test(part)) &&
    parts.every(
      (part) =>
        delta.test(part) ||
        part ===
          "Important: Do not attempt to call tools that are no longer available unless you've been notified that they're available again.",
    )
  );
}
