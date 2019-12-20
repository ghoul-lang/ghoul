// Copyright (C) 2019 Michael Brandon Huddle
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

#include <AST/Types/BuiltInType.hpp>
#include "CastHelper.hpp"

gulc::CastSide gulc::CastHelper::getSideToCast(gulc::BuiltInType *left, gulc::BuiltInType *right) {
    bool leftIsSigned = left->isSigned();
    bool rightIsSigned = right->isSigned();
    bool leftIsFloating = left->isFloating();
    bool rightIsFloating = right->isFloating();
    std::size_t leftSize = left->size();
    std::size_t rightSize = right->size();

    // First we check if either side is floating. If one side is floating we have to cast the other to be floating
    if (leftIsFloating) {
        if (rightIsFloating) {
            // If right is also floating we go based on which is bigger
            goto getSideBySize;
        } else {
            // If right is not floating we have to cast the right side
            return CastSide::Right;
        }
    } else if (rightIsFloating) {
        // Left is guaranteed to not be floating so we have to cast the left side
        return CastSide::Left;
    } else {
        // Next we have to check if either side is signed. If one side is signed we have to cast the other to be signed
        if (leftIsSigned) {
            if (rightIsSigned) {
                // If right is also signed we go based on which is bigger
                goto getSideBySize;
            } else {
                // If right is not signed we have to cast the right side
                return CastSide::Right;
            }
        } else if (rightIsSigned) {
            // Left is guaranteed to not be signed so we have to cast the left side
            return CastSide::Left;
        } else {
            // If neither are signed we go by size
            goto getSideBySize;
        }
    }

getSideBySize:
    if (leftSize == rightSize) {
        // If both sides are the same we don't cast
        return CastSide::Neither;
    } else if (leftSize > rightSize) {
        // We cast the right side if left is bigger. We DO NOT narrow the type.
        return CastSide::Right;
    } else {
        // If left is smaller than right then we cast the left side
        return CastSide::Left;
    }
}
